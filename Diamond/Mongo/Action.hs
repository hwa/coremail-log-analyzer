module Diamond.Mongo.Action
       (withDB
       ,insertDB
       ,repsertDB
       ,createConnectionPool
       ,running
        )
       where

import Database.MongoDB
import Data.Pool
import Control.Concurrent (forkIO)
import Control.Exception
import System.IO (hPutStrLn, stderr)


-- | within the context of database
withDB :: Database -> Action IO b -> IO b
withDB db action = do
  pipe <- connect (host "127.0.0.1")
  e <- access pipe master db action
  close pipe
  return e

-- | create a connection pool
createConnectionPool :: String -> Int -> IO (Pool Pipe)
createConnectionPool dbHost poolSize = do
  let conn = do
        print "connecting"
        catch (connect (host dbHost)) (\e -> do
                                          let err = show (e :: IOException)
                                          hPutStrLn stderr ("warn: connect, " ++ err)
                                          ioError e)
  let close pipe = do
        print "closing"
        close pipe
  createPool conn close 1 1 poolSize

running :: Pool Pipe -> Database -> Action IO b -> IO ()
running pool db action = do
  let r = \pipe -> do
        let a = (access pipe master db action >> return ())
        catch a  (\e -> do
                     let err = show (e :: IOException)
                     hPutStrLn stderr ("warn: " ++ err)
                     close pipe)
  let wr = withResource pool r
  forkIO $ wr
  return ()

insertDB :: Pool Pipe -> Database -> Collection -> (a -> IO Document) -> a -> IO ()
insertDB pool db collection convert record = do
  doc <- convert record
  running pool db $ insert_ collection doc
  return ()

-- use insert


-- | print the documents to stdout
-- printDocs :: String -> [Document] -> IO ()
-- printDocs title docs = putStrLn title >> mapM_ (print . exclude ["_id"]) docs

-- | query all documents in a collection
-- allDocs :: (MonadIO m, MonadBaseControl IO m) =>
--            Collection -> Action m [Document]
-- allDocs coll= rest =<< find (select [] coll)

insertDB' :: Pipe -> Database -> Collection -> (a -> IO Document) -> a -> IO ()
insertDB' pipe db collection convert record = do
  doc <- convert record
  --return ()
  --print doc
  --print . length $ doc
  access pipe master db $ insert_ collection doc
  return ()
  --return $ either (error . show) id e

repsertDB :: Pipe -> Database -> Selection -> (a -> IO Document) -> a -> IO ()
repsertDB pipe db sel convert record = do
  doc <- convert record
  --print . length $ doc
  --return ()
  --print doc
  --putStrLn $ (show . coll $ sel) ++ (show . length $ doc)
  access pipe master db $ repsert sel doc
  return ()
  --return $ either (error . show) id e
