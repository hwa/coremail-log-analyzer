{-# LANGUAGE OverloadedStrings #-}

-- | 日志文件转换为流
-- 日志文件可能定期滚动，故需转换为恒流。策略是，持续读取转为流，一旦遇
-- 到读取异常，则重开文件，若所断处内容与前不一致，则是遇到滚缩，需从头读
-- 取，若一致则可能是真正的读取异常，则从断处继续读取。

module Diamond.Streams.LogFile
       (lineStream,
        bytesStream,
        textStream,
        mtaBSStream,
        mtaTextStream,
        logDate
       )
       where

import qualified System.IO.Streams as S
import System.IO
import System.IO.Error
import Control.Applicative (pure)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Control.Exception (catch)
import Codec.Text.IConv
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import System.Directory (getDirectoryContents)
import Data.List (sort)
import Data.IORef
import System.Posix.Files (getFileStatus, modificationTime)
import System.Posix.Types (EpochTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time (toGregorian, utctDay)
import Control.Concurrent (threadDelay)

-- | 持续读行
waitLine :: Handle -> IO BS.ByteString
waitLine handle = catch (BS.hGetLine handle) ifEOF
  where
    ifEOF e = if isEOFError e
              then threadDelay (10*10^6) >> waitLine handle
              else ioError e

-- | 文件转为源流
-- 每次流出一行
-- 若遇到文末，则等待
-- 若途中文件关闭，则永远等待

lineStream :: Handle -> IO (S.InputStream BS.ByteString)
lineStream handle = S.makeInputStream $ do
  line <- waitLine handle
  return $ Just line


-- | 持续读字节
bUFSIZ :: Int
bUFSIZ = 32752

waitBytes :: Handle -> IO BS.ByteString
waitBytes handle = do
  bytes <- BS.hGetSome handle bUFSIZ
  if BS.null bytes
    then waitBytes handle
    else return bytes

-- |
bytesStream :: Handle -> IO (S.InputStream BS.ByteString)
bytesStream handle = S.makeInputStream $ do
  bytes <- waitBytes handle
  return $ Just bytes


-- | ByteString转编码
transEncode :: EncodingName -> EncodingName -> BS.ByteString -> BS.ByteString
transEncode inEnc outEnc =
  BS.concat . BL.toChunks . convert' . BL.fromChunks . pure
  where
    convert' = convertFuzzy Transliterate inEnc outEnc

-- | Bytestring转为Text
bsToText :: EncodingName -> BS.ByteString -> Text
bsToText "UTF8" = decodeUtf8With lenientDecode
bsToText inEnc =
  decodeUtf8 . transEncode inEnc "UTF8"

bsStreamToTextStream :: EncodingName
                     -> S.InputStream BS.ByteString
                     -> IO (S.InputStream Text)
bsStreamToTextStream inEnc bsStream = do
  lnStream <- S.lines bsStream >>= S.map (`BS.append` "\n")
  S.map (bsToText inEnc) lnStream

-- | 日志的Text流
textStream :: EncodingName -> Handle -> IO (S.InputStream Text)
textStream enc handle =
  bytesStream handle >>= bsStreamToTextStream enc



-- | MTA分时日志文件接续为持续流

mtaTextStream :: FilePath -> EncodingName -> IO (S.InputStream Text)
mtaTextStream mtaDir enc =
  mtaBSStream mtaDir >>= bsStreamToTextStream enc

mtaBSStream :: FilePath -> IO (S.InputStream BS.ByteString)
mtaBSStream mtaDir = do
  dir <- currentDir mtaDir
  h <- openFile (dir ++ "/mta_00_00_00.log") ReadMode
  handleR <- newIORef h
  nR <- newIORef (8 :: Int)
  S.makeInputStream (readMTABS dir nR handleR)

currentDir :: FilePath -> IO FilePath
currentDir mtaDir =
  withFile scan_log ReadMode $
    \h -> do
      [cd, _, _] <- fmap lines . hGetContents $ h
      return (mtaDir ++ "/" ++ cd)
  where
    scan_log = mtaDir ++ "/mta.scan.log"

readMTABS :: FilePath -> IORef Int -> IORef Handle -> IO (Maybe BS.ByteString)
readMTABS logdir nR handleR = do
  handle <- readIORef handleR
  bytes <- BS.hGetSome handle bUFSIZ
  if BS.null bytes
    then hitEnd
    else return (Just bytes)
  where
    hitEnd = do
      n <- readIORef nR
      handle <- readIORef handleR
      files <- fmap (tail .tail . sort) $ getDirectoryContents logdir
      if (1 + n) == length files
        then readMTABS logdir nR handleR
        else do
        hClose handle
        h <- openFile (logdir ++ "/" ++ (files !! (n+1))) ReadMode
        writeIORef handleR h
        writeIORef nR (n+1)
        readMTABS logdir nR handleR

logDate :: FilePath -> IO (Int, Int, Int)
logDate logFile = do
  (y, m, d) <- fmap (dateOfCTime . (+ 8*3600) . modificationTime) $ getFileStatus logFile
  return (fromIntegral y, m, d)

dateOfCTime :: EpochTime -> (Integer, Int, Int)
dateOfCTime t =
  toGregorian . utctDay . posixSecondsToUTCTime . realToFrac $ t
