{-# LANGUAGE OverloadedStrings #-}
module Diamond.Mongo.Json
       (tojson)
       where

import           Data.Aeson
import qualified Data.ByteString.Lazy as L
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

import qualified Diamond.Parser.DALog as DA
import qualified Diamond.Parser.MTALog as MTA
import           Diamond.Utils.DateTime
import           Diamond.LogRecord

instance ToJSON DA.LogRecord where
  toJSON (DA.LogRecord time flag detail) =
    Object $ Map.union o1 (detailing detail)
    where
      o1 = Map.fromList [("time", toJSON time), ("flag", toJSON flag)]
      detailing (DA.Delivery m) = Map.map toJSON m
      detailing o = Map.fromList [("unparsed", toJSON (show o))]

instance ToJSON MTA.LogRecord where
  toJSON (MTA.LogRecord time record) =
    Object $ Map.union o1 o2
    where
      o1 = Map.singleton "time" (toJSON time)
      o2 = Map.map toJSON record

tojson :: (LogRecord a, ToJSON a) => (Int, Int, Int) -> String -> a -> Value
tojson (year, month, day) servername logrecord =
  Object $ Map.union o1 o2
  where
    Object o1 = toJSON logrecord
    [hour, minute, second] = recordTime logrecord
    unixSeconds = toUnixSeconds (year, month, day) (hour, minute, second)
    o2 = Map.fromList [("timestamp", toJSON unixSeconds),
                       ("date", toJSON (year, month, day)),
                       ("mailserver", toJSON servername)]
