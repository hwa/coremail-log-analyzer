{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Diamond.Mongo.PyAPI
       (insert
       ,repsert'
       ,remove)
       where

-- import Network.Http.Client
-- import Data.ByteString

-- insert :: ByteString -> ByteString -> IO ()
-- insert logtype logjson = do
--   postForm url [("logjson", logjson)] (\_ _ -> return ())
--   where
--     url = "http://localhost:8080/insert/" `append` logtype


import Network.HTTP
import Network.URI
import Data.ByteString.Lazy
import Prelude hiding (length)
import Data.Aeson
import Control.Concurrent (threadDelay)
import Control.Exception (catch, SomeException)


simpleHTTP' :: HStream ty => Request ty -> IO ()
simpleHTTP' rq =
  catch (go rq) (\(e::SomeException) -> do
                    print (show e)
                    threadDelay (10^6)
                    simpleHTTP' rq)
    where
      go r = simpleHTTP r >>= print

rq :: String -> ByteString -> Request ByteString
rq logtype logjson =
  Request url POST headers logjson
  where
    headers = [mkHeader HdrContentType "application/json",
               mkHeader HdrContentLength (show . length $ logjson)]

    Just url = parseURI $ "http://127.0.0.1:8080/insert/" ++ logtype

insert :: String -> ByteString -> IO ()
insert collection docjson = do
  let r = rq collection docjson
  simpleHTTP' r
  return ()

rq2 :: String -> Value -> Value -> Request ByteString
rq2 collection selection document =
  Request url POST headers repsertJson
  where
    repsertJson = encode $ object ["s" .= selection, "d" .= document]
    headers = [mkHeader HdrContentType "application/json",
               mkHeader HdrContentLength (show . length $ repsertJson)]

    Just url = parseURI $ "http://127.0.0.1:8080/repsert/" ++ collection

repsert' :: String -> Value -> Value -> IO ()
repsert' collection selection document = do
  let r = rq2 collection selection document
  simpleHTTP' r
  return ()

rq3 :: String -> Value -> Request ByteString
rq3 collection selection =
  Request url POST headers selectionJson
  where
    selectionJson = encode selection
    headers = [mkHeader HdrContentType "application/json",
               mkHeader HdrContentLength (show . length $ selectionJson)]
    Just url = parseURI $ "http://127.0.0.1:8080/remove/" ++ collection

remove :: String -> Value -> IO ()
remove collection selection = do
  let r = rq3 collection selection
  simpleHTTP' r
  return ()
