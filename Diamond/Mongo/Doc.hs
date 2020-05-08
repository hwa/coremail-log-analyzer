{-# LANGUAGE OverloadedStrings #-} --, TypeSynonymInstances #-}
module Diamond.Mongo.Doc
       (daToDoc,
        mtaToDoc,
        mapToDoc,
        datingDoc,
        addHostname,
        fieldConcat,
        hasField)
       where

import qualified Diamond.Parser.DALog as DA
import qualified Diamond.Parser.MTALog as MTA
import           Diamond.Utils.DateTime
import           Prelude hiding (lookup)
import           Data.Bson
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
--import           Data.UString (u)
--import qualified Data.ByteString as BS
--import qualified Data.Text as T
--import           Data.Text (Text)
--import qualified Data.CompactString.UTF8 as CS
import           Data.Maybe

type Map k a = HashMap k a

-- instance Val BS.ByteString where
--   val = String . u . map (toEnum . fromEnum) . BS.unpack
--   cast' (String s) = Just . BS.pack . map (toEnum . fromEnum) . CS.unpack $ s
--   cast' _ = Nothing
-- instance Val T.Text where
--   val = String . u . T.unpack
--   cast' (String s) = Just . T.pack . CS.unpack $ s
--   cast' _ = Nothing

-- class Labels l where
--   lab :: l -> Label

-- instance Labels Label where
--   lab = id
-- instance Labels String where
--   lab = u
-- instance Labels Text where
--   lab = u . T.unpack


daToDoc :: DA.LogRecord -> Document
daToDoc (DA.LogRecord time flag detail) =
  ["time" =: time, "flag" =: flag] ++ detailing detail
    where
      detailing (DA.Delivery m) = mapToDoc m
      detailing o = ["unparsed" =: show o]

mtaToDoc :: MTA.LogRecord -> Document
mtaToDoc (MTA.LogRecord time record) =
  ("time" =: time) : mapToDoc record

mapToDoc :: (Val a) => Map Label a -> Document
mapToDoc = Map.foldrWithKey (\k a z -> (k =: a) : z) []




-----------------------------
-- | 原始日誌無日期，用此增設
-- datingDoc :: Document -> IO Document
-- datingDoc doc = do
--   let [h, m, s] = at "time" doc
--   newTime <- MTA.fixDate (h, m, s)
--   let newTimeField = "time" =: newTime
--   return $ merge [newTimeField] doc
datingDoc :: (Int, Int, Int) -> Document -> IO Document
datingDoc (y, m, d) doc = do
  let [h, minu, s] = at "time" doc
  let newTimeField = "time" =: toUnixSeconds (y,m,d) (h,minu,s)
  return $ merge [newTimeField] doc

addHostname :: (Val a) => a -> Document -> Document
addHostname hostname = merge ["facet-host" =: hostname]

fieldConcat :: (Val a) => Label -> [Document] -> [a]
fieldConcat field docs = docs >>= lookup field


------------
--
hasField :: Label -> Document -> Bool
hasField fieldname doc = isJust $ look fieldname doc
-- hasField = (isJust .) . look
