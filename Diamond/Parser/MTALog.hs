{-# LANGUAGE OverloadedStrings #-}
module Diamond.Parser.MTALog
       (
         LogRecord (..),
         logTime,
         record,
         --record',
         --fixDate
--       parseFiles'take,
--       parseFiles'take'transf,
--       histoGram
       )
       where

import qualified Data.Attoparsec.Text as AtT
--import qualified Data.Attoparsec.Types as At (Parser)
--import           Control.Applicative (Alternative(..), Applicative(..), (<$>))
import           Control.Applicative (Applicative(..), (<$>))
--import qualified Data.ByteString as BS
import           Data.Text (Text)
--import qualified Data.Text as T
--import qualified Data.Text.Encoding as T
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
--import           Data.Char (chr, ord)
--import           System.IO (withFile, IOMode(..))
--import           Data.Time
--import           Diamond.Utils.DateTime

type Map k a = HashMap k a

data LogRecord = LogRecord {
  time :: [Int],
  detail :: Map Text Text
  } deriving Show


logTime :: AtT.Parser [Int]
logTime = combine <$>
               AtT.digit <*> AtT.digit <* AtT.char ':' <*>
               AtT.digit <*> AtT.digit <* AtT.char ':' <*>
               AtT.digit <*> AtT.digit
               where
                 combine h1 h2 m1 m2 s1 s2 = [toI h1 h2,
                                              toI m1 m2,
                                              toI s1 s2]
                 toI x y = read [x,y] :: Int

record :: AtT.Parser LogRecord
record = (\t r -> LogRecord t $ Map.fromList r) <$>
           logTime
           <* AtT.string " ["
           <*> AtT.sepBy p'cmd (AtT.char ',')
           <* AtT.char ']'
           <* AtT.endOfLine
             where
               p'cmd = (\x y -> (x, y)) <$>
                       AtT.takeWhile (AtT.notInClass ":]")
                       --AtT.takeWhile (AtT.notInClass ":")
                    <* AtT.char ':'
                   <*> AtT.scan 'x' tran
                   where
                     tran '\\' ']' = Just ']'
                     tran _ ']' =  Nothing
                     tran '\\' ',' = Just ','
                     tran _ ',' = Nothing
                     tran _ x = Just x

--
-- escape escChar = AtT.scan 'a' p
--    where
--         p '\\' ',' = Just ','
--         p _ ',' = Nothing
--         p _ x = Just x

-- ---------------------------------------------------
-- -- | provided time on log, guess the correct date
-- --   assuming there is little diff time between mail
-- --   server and local host
-- fixDate' :: (Int, Int, Int) -> IO (Int, Int, Int)
-- fixDate' (h, m, s) = do
--   cT <- getCurrentTime
--   let cD = utctDay cT
--   let ct = utctDayTime cT
--   let fs = (h - 8) * 3600 + m * 60 + s
--   let dt = (toRational fs) - (toRational ct)
--   let dd | dt > 82800 = -1      -- 23 hours
--          | dt < -82800 = 1
--          | otherwise = 0
--   let (y, mon, d) = toGregorian $ addDays dd cD
--   return (fromInteger y, mon, d)

-- --------------------------------------------------
-- -- | provided time on log, guess the correct
-- --   unix epoch seconds
-- fixDate :: (Int, Int, Int) -> IO Int
-- fixDate (hour, minute, second) = do
--   (year, month, day) <- fixDate' (hour, minute, second)
--   return $ toUnixSeconds (year, month, day) (hour, minute, second)




-- |
--
-- type LogLine = Text

-- --type Parser = At.Parser RawText

-- record' :: At.Parser LogLine LogRecord
-- record' = record


--
-- Tests
-- testparse ttype parser =
--   withFile filename ReadMode BS.hGetLine >>= return . T.decodeUtf8 >>=
--   doparse ttype
--     where
--       doparse "only" = print . AtT.parseOnly parser
--       doparse "all" = AtT.parseTest parser
--       filename = "/home/cch/hscode/log-analytics/logs/61log/mta_00_00_00.log"



-- if' :: Bool -> a -> a -> a
-- if' True  x _ = x
-- if' False _ y = y

-- numBS :: BS.ByteString -> Float
-- numBS = read . map (chr . fromIntegral) . BS.unpack
