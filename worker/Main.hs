{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
       where

import           System.IO
import qualified System.IO.Streams as S
import qualified Data.Attoparsec.Text as At
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.Hashable (Hashable)
import           Data.Text (Text)
import qualified Data.Text as T (pack, map)
import           Database.MongoDB (connect, host,
                                   access, master, close,
                                   Pipe, Database, Collection, Document,
                                   insert_, repsert, Selection, select,
                                   merge, (=:))
import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar
import           Data.Aeson (encode, toJSON, object, (.=))
import           Data.ByteString.Lazy (toStrict)

import qualified Diamond.Parser.MTALog as MTA
import qualified Diamond.Parser.DALog as DA
import           Diamond.Analytics.Statistics
import           Diamond.Streams.LogFile
import           Diamond.Mongo.Doc
import           Diamond.Utils.DateTime
import           Diamond.Mongo.Action
import           Diamond.Mongo.Json
import           Diamond.Mongo.PyAPI
import           Diamond.Redis
import           Data.Pool
import           Database.Redis (Connection)


type Map k a = HashMap k a

mapKeys :: (Eq k2, Hashable k2) => (k1 -> k2) -> Map k1 a -> Map k2 a
mapKeys f = Map.fromList . Map.foldrWithKey (\k x xs -> (f k, x) : xs) []

parseStream :: At.Parser a
               -> S.InputStream Text
               -> IO (S.InputStream a)
parseStream parser stream =
  S.map (At.parseOnly parser) stream >>=
  S.filter c >>=
  S.map m
  where
    c = either (const False) (const True)
    m (Right a) = a
    m (Left _) = error "should be impossible"

printS :: (Show a) => S.InputStream a -> IO ()
printS s = S.read s >>= print >> printS s

daStream :: FilePath -> IO (S.InputStream DA.LogRecord)
daStream logdir = do
  h <- openFile (logdir ++ "/deliveragent.log") ReadMode
  ts <- textStream "UTF8" h
  parseStream DA.record ts



mtaStream :: FilePath -> IO (S.InputStream MTA.LogRecord)
mtaStream logdir = do
  let mtadir = logdir ++ "/mtatrans"
  ts <- mtaTextStream mtadir "UTF8"
  parseStream MTA.record ts


data Counters = Counters {failUserCnt :: FieldCounter Text
                         ,failIPCnt :: FieldCounter Text
                         ,senderCnt :: FieldCounter Text
                         ,rcptCnt :: FieldCounter Text
                         ,scoreCnt :: FieldCounter Float
                         ,sizeCnt :: FieldCounter Float}
emptyCounters :: Counters
emptyCounters = Counters
                  Map.empty Map.empty Map.empty
                  Map.empty Map.empty Map.empty

countMTA :: Counters -> MTA.LogRecord -> Counters
countMTA (Counters uc ipc a1 a2 a3 a4) mtaRecord =
  let
      (uc', ipc') = countFailAuth (uc, ipc) mtaRecord
  in
   Counters uc' ipc' a1 a2 a3 a4

countDA :: Counters -> DA.LogRecord -> Counters
countDA (Counters a1 a2 sc rc scc szc) daRecord =
  let sc' = countSender sc daRecord
      rc' = countRcpt rc daRecord
      scc' = countScore scc daRecord
      szc' = countSize szc daRecord
  in
   Counters a1 a2 sc' rc' scc' szc'

repsertCounters' :: (Int, Int, Int) -> Counters -> IO Counters
repsertCounters' logdate cs@(Counters uc ipc sc rc scc szc) = do
  let tf = Map.singleton "date" (toUnixSeconds logdate (0,0,0))
      mk = mapKeys (T.pack . show)
      undot = mapKeys (\k -> let f c = if c == '.' then '\xff0e' else c in T.map f k)
  repsert' "counter_failauth_user" (toJSON tf) (toJSON $ Map.union tf (undot uc))
  repsert' "counter_failauth_ip" (toJSON tf) (toJSON $ Map.union tf (undot ipc))
  repsert' "counter_sent" (toJSON tf) (toJSON $ Map.union tf (undot sc))
  repsert' "counter_rcpt" (toJSON tf) (toJSON $ Map.union tf (undot rc))
  repsert' "counter_score" (toJSON tf) (toJSON $ Map.union tf (undot . mk $ scc))
  repsert' "counter_size" (toJSON tf) (toJSON $ Map.union tf (undot . mk $ szc))
  return cs



repsertCounters :: Pipe -> Database -> (Int, Int, Int) -> Counters -> IO Counters
repsertCounters pipe db logdate cs@(Counters uc ipc sc rc scc szc) = do
  let tf = ["date" =: toUnixSeconds logdate (0,0,0)]
  repsertDB pipe db (select tf "counter_failauth_user") (timing tf) uc
  repsertDB pipe db (select tf "counter_failauth_ip") (timing tf) ipc
  repsertDB pipe db (select tf "counter_sent") (timing tf) sc
  repsertDB pipe db (select tf "counter_rcpt") (timing tf) rc
  repsertDB pipe db (select tf "counter_score") (timing tf) scc
  repsertDB pipe db (select tf "counter_size") (timing tf) szc
  return cs
  where
    timing tf = return . merge tf . mapToDoc . mapKeys (T.pack . show)

reduceMap :: (Eq k) => [k] -> Map k v -> Map k v
reduceMap ks = (Map.filterWithKey (\k _ -> k `elem` ks))

reduceMTARecord :: [Text] -> MTA.LogRecord -> MTA.LogRecord
reduceMTARecord ks (MTA.LogRecord t d)  =
  MTA.LogRecord t (reduceMap ks d)

reduceDARecord :: [Text] -> DA.LogRecord -> DA.LogRecord
reduceDARecord ks (DA.LogRecord t f d) =
  DA.LogRecord t f (c d)
  where
    c (DA.Delivery m) = DA.Delivery (reduceMap ks m)
    c x = x

checkFailCounter :: String -> MTA.LogRecord -> Counters -> IO Counters
checkFailCounter logHost r@(MTA.LogRecord _ m) c@(Counters failuser failip _ _ _ _) = do
  case (Map.lookup "AuthUser" m) of
    Nothing -> return ()
    Just user -> case (Map.lookup user failuser) of
      Nothing -> return ()
      Just t -> if t > 100 then save1 user t else return ()
  case (Map.lookup "ip" m) of
    Nothing -> return ()
    Just ip -> case (Map.lookup ip failip) of
      Nothing -> return ()
      Just t2 -> if t2 > 100 then save2 ip t2 else return ()
  return c
  where
    save1 user t = repsert' "warn_failuser" (object ["mail_server" .= logHost, "user" .= user]) (object ["user" .= user, "count" .= t, "mail_server" .= logHost])
    save2 ip t = repsert' "warn_failip" (object ["mail_server" .= logHost, "ip" .= ip]) (object ["ip" .= ip, "count" .= t, "mail_server" .= logHost])

countMTA' :: Pool Connection-> MTA.LogRecord -> IO ()
countMTA' redis r@(MTA.LogRecord _ m) = do
  case (Map.lookup "AuthUser" m) of
    Nothing -> return ()
    Just user -> incCounter redis "failuser" user
  case (Map.lookup "ip" m) of
    Nothing -> return ()
    Just ip -> incCounter redis "failip" ip
  return ()

domta :: Pool Pipe -> Pool Connection -> String -> String -> Database -> FilePath -> IO ()
domta pool redis logHost dbHost dbname logsdir = do
  let (logdir :: FilePath) = logsdir ++ "/" ++ logHost
  let mtadir = logdir ++ "/mtatrans"
  s <- mtaStream logdir
  logdate <- logDate mtadir
  --remove "mta" (object ["date" .= logdate])
  --let insertRecord = insert "mta" . encode . tojson logdate logHost
  let insertRecord = insertDB pool dbname "mta" (datingDoc logdate . addHostname logHost . mtaToDoc)
  s1 <- S.mapM_ (\r -> insertRecord r >> countMTA' redis r) s
  --let ks = ["ip", "AuthUser", "cmd", "Result"]
  --s1' <- S.map (reduceMTARecord ks) s1
  --countersM <- newMVar emptyCounters
  --(s2, _) <- S.inputFoldM (\cm r -> modifyMVar_ cm (\c -> return (countMTA c r)) >> withMVar countersM (repsertCounters' logdate) >> return cm) countersM s1
  --(s2, _) <- S.inputFoldM (\c r -> checkFailCounter logHost r (countMTA c r)) emptyCounters s1
  nullout <- S.nullOutput
  S.connect s1 nullout
  return ()

checkSentCounter :: String -> DA.LogRecord -> Counters -> IO Counters
checkSentCounter logHost r@(DA.LogRecord _ _ (DA.Delivery m)) c@(Counters _ _ sent _ _ _) = do
  case (Map.lookup "from" m) of
    Nothing -> return ()
    Just from -> case (Map.lookup from sent) of
      Nothing -> return ()
      Just s -> if s > 100 then save from s else return ()
  return c
  where
    save from s = repsert' "warn_sent" (object ["mail_server" .= logHost, "from" .= from]) (object ["from" .= from, "count" .= s, "mail_server" .= logHost ])
checkSentCounter _ _ c = return c

countDA' :: Pool Connection -> DA.LogRecord -> IO ()
countDA' redis r@(DA.LogRecord _ _ (DA.Delivery m)) = do
  case (Map.lookup "from" m) of
    Nothing -> return ()
    Just from -> incCounter redis "sender" from
countDA' _ _ = return ()

doda :: Pool Pipe -> Pool Connection -> String -> String -> Database -> FilePath -> IO ()
doda pool redis logHost dbHost dbname logsdir = do
  let (logdir :: FilePath) = logsdir ++ "/" ++ logHost
  s <- daStream logdir
  logdate <- logDate logdir
  --remove "da" (object ["date" .= logdate])
  --let insertRecord = insert "da"  . encode . tojson logdate logHost
  let insertRecord = insertDB pool dbname "da" (datingDoc logdate . addHostname logHost . daToDoc)
  s1 <- S.mapM_ (\r -> insertRecord r >> countDA' redis r) s
  --let ks = ["from", "to", "state", "score", "size"]
  --s1' <- S.map (reduceDARecord ks) s1
  --countersM <- newMVar emptyCounters
  --(s2, _) <- S.inputFoldM (\cm r -> modifyMVar_ cm (\c -> return (countDA c r)) >> withMVar countersM (repsertCounters' logdate) >> return cm) countersM s1
  --(s2, _) <- S.inputFoldM (\c r -> checkSentCounter logHost r (countDA c r)) emptyCounters s1
  nullout <- S.nullOutput
  S.connect s1 nullout
  return ()

dohost :: String -> IO ()
dohost logHost = do
  let dbHost = "127.0.0.1"
      dbname = "diamond"
      logDir = "/root/diamond/logs"
      poolSize = 300
  pool <- createConnectionPool dbHost poolSize
  redis <- createRedisPool poolSize
  _ <- forkIO $ domta pool redis logHost dbHost dbname logDir
  doda pool redis logHost dbHost dbname logDir

main :: IO ()
main = do
  delCounter'  "sender"
  delCounter' "failuser"
  delCounter' "failip"
  _ <- forkIO $ dohost "app1"
  _ <- forkIO $ dohost "app2"
  dohost "app3"
