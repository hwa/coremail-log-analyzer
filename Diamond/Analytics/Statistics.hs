{-# LANGUAGE OverloadedStrings #-}
module Diamond.Analytics.Statistics
       (FieldCounter
       ,countField
       ,countSender
       ,countRcpt
       ,countState
       ,countScore
       ,countSize
       ,countFailAuth
       ,stepizeCounter
       )
       where

import qualified Diamond.Parser.MTALog as MTA
import qualified Diamond.Parser.DALog as DA
--import           Data.Map.Strict (Map)
--import qualified Data.Map.Strict as Map
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.Hashable (Hashable)
import           Data.Text (Text)
import qualified Data.Text as T
--import           Data.Maybe (fromJust)

-- DA

type Map k a = HashMap k a

-- |
defered :: DA.Detail -> Bool
defered (DA.Delivery mapping) =
  maybe False (== "defer") $ Map.lookup "state" mapping
defered _ = False


-- | check appending text
checkAppendText :: (Text -> Bool) -> DA.Detail -> Bool
checkAppendText check (DA.Delivery mapping) =
  maybe False check $ Map.lookup "append" mapping
checkAppendText _ _ = False


-- block entry

-- | blocked for no PTR record
noPTR :: DA.Detail -> Bool
noPTR = checkAppendText check
  where
    keywords = ["Client host rejected: cannot find your hostname",
                "Cannot resolve PTR record",
                "Error checking reverse for"]
    check txt = any (`T.isInfixOf` txt) keywords

-- | blocked by antispam union
spamList :: DA.Detail -> Bool
spamList = checkAppendText check
  where
    keywords = ["anti-spam.org",
                "blocked"]
    check txt = any (`T.isInfixOf` txt) keywords


-- counters

type FieldCounter k = Map k Int

mapKeysWith :: (Eq k2, Hashable k2) =>
               (a -> a -> a) -> (k1 -> k2) -> Map k1 a -> Map k2 a
mapKeysWith c f =
  Map.fromListWith c . Map.foldrWithKey (\k x xs -> (f k, x) : xs) []

-- | count distribution of a field
-- when counting a record without the field
-- exception be raised because use of "fromJust"
countField :: (Eq k, Hashable k) =>
              Text                    -- field name
              -> (Text -> k)          -- function to parse field value
              -> FieldCounter k
              -> DA.LogRecord
              -> FieldCounter k
countField _ _ c (DA.LogRecord _ _ (DA.Others _)) = c
countField field parse counter (DA.LogRecord _ _ (DA.Delivery mapping)) =
  c value
  where
    value = Map.lookup field mapping
    c Nothing = counter
    c (Just value') = Map.insertWith (+) (parse value') 1 counter


countSender :: FieldCounter Text -> DA.LogRecord -> FieldCounter Text
countSender = countField "from" id

countRcpt :: FieldCounter Text -> DA.LogRecord -> FieldCounter Text
countRcpt = countField "to" id

countState :: FieldCounter Text -> DA.LogRecord -> FieldCounter Text
countState = countField "state" id

countScore :: FieldCounter Float -> DA.LogRecord -> FieldCounter Float
countScore = countField "score" (read . T.unpack)

countSize :: FieldCounter Float -> DA.LogRecord -> FieldCounter Float
countSize = countField "size" (read . T.unpack)

stepizeCounter :: (Eq k1, Eq k2, Hashable k1, Hashable k2) =>
                  (k1 -> k2)
                  -> FieldCounter k1
                  -> FieldCounter k2
stepizeCounter = mapKeysWith (+)

-- | count failed auth
countFailAuth :: (FieldCounter Text,        -- user counter
                  FieldCounter Text)        -- ip counter
                 -> MTA.LogRecord
                 -> (FieldCounter Text, FieldCounter Text)
countFailAuth (userCounter, ipCounter) (MTA.LogRecord _ mapping) =
  c cmd result user ip
  where
    cmd = Map.lookup "cmd" mapping
    result = Map.lookup "Result" mapping
    user = Map.lookup "AuthUser" mapping
    ip = Map.lookup "ip" mapping
    inc k = Map.insertWith (+) k 1
    c (Just cmd') (Just result') (Just user') (Just ip')
      | cmd' == "AUTH" && result' == "Failed" = (inc user' userCounter,
                                                 inc ip' ipCounter)
      | otherwise = (userCounter, ipCounter)
    c _ _ _ _ = (userCounter, ipCounter)
