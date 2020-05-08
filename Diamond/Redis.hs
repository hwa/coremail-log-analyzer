 {-# LANGUAGE OverloadedStrings #-}

module Diamond.Redis
       (incCounter
       ,getCounter
       ,delCounter
       ,incCounter'
       ,getCounter'
       ,delCounter'
       ,createRedisPool)
       where

import Database.Redis
import Data.ByteString
import Data.Text (Text)
import Data.Text.Encoding
import Data.Pool
--import Control.Monad.Trans (liftIO)

createRedisPool :: Int -> IO (Pool Connection)
createRedisPool poolSize = do
  let us = UnixSocket "/var/run/redis/redis.sock"
      conn = connect $ defaultConnectInfo {connectPort = us}
      close = \c -> runRedis c (quit >> return ())
      poolSize = 100
  createPool conn close 1 10 poolSize


incCounter :: Pool Connection -> Text -> Text -> IO ()
incCounter pool counter key = do
  let counter' = encodeUtf8 counter
      key' = encodeUtf8 key
  withResource pool $ \conn -> runRedis conn $ do
    _ <- hincrby counter' key' 1
    return ()

getCounter :: Pool Connection -> Text -> Text -> IO (Maybe ByteString)
getCounter pool counter key = do
  let counter' = encodeUtf8 counter
      key' = encodeUtf8 key
  withResource pool $ \conn -> runRedis conn $ do
    Right m <- hget counter' key'
    return m

delCounter :: Pool Connection -> Text -> IO ()
delCounter pool counter = do
  let counter' = encodeUtf8 counter
  withResource pool $ \conn -> runRedis conn $ do
    Right _ <- del [counter']
    return ()


incCounter' :: Text -> Text -> IO ()
incCounter' counter key = do
  let counter' = encodeUtf8 counter
      key' = encodeUtf8 key
  let us = UnixSocket "/var/run/redis/redis.sock"
  conn <- connect $ defaultConnectInfo {connectPort = us}
  runRedis conn $ do
    _ <- hincrby counter' key' 1
    quit
    return ()


getCounter' :: Text -> Text -> IO (Maybe ByteString)
getCounter' counter key = do
  let counter' = encodeUtf8 counter
      key' = encodeUtf8 key
  let us = UnixSocket "/var/run/redis/redis.sock"
  conn <- connect $ defaultConnectInfo {connectPort = us}
  runRedis conn $ do
    Right m <- hget counter' key'
    quit
    return m

delCounter' :: Text -> IO ()
delCounter' counter = do
  let counter' = encodeUtf8 counter
  let us = UnixSocket "/var/run/redis/redis.sock"
  conn <- connect $ defaultConnectInfo {connectPort = us}
  runRedis conn $ do
    Right _ <- del [counter']
    quit
    return ()
