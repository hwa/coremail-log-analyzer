{-# LANGUAGE OverloadedStrings #-}
module Diamond.Parser.DALog
       (LogRecord (..),
        Detail (..),
        record
       )
       where

import qualified Data.Attoparsec.Text as AtT
import Control.Applicative (Alternative(..), Applicative(..), (<$>))
--import qualified Data.ByteString as BS
import           Data.Text (Text)
--import qualified Data.Text as T
--import qualified Data.Text.Encoding as T
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Diamond.Parser.MTALog (logTime)

type Map k a = HashMap k a

data LogRecord = LogRecord {
     time :: [Int],
     flag :: Text,
     detail :: Detail
     } deriving Show

data Detail = Delivery (Map Text Text)
              | Others Text
              deriving Show


record :: AtT.Parser LogRecord
record = LogRecord <$> p'time <*> p'flag <*> p'detail
         where
         p'time = AtT.string "T:" *> AtT.many1 AtT.digit *> AtT.char '('
                  *> logTime <* AtT.char ')'
         p'flag = AtT.char '[' *> AtT.takeWhile (/= ']') <* AtT.char ']' <* AtT.char ' '
         p'detail = AtT.try p'delivery <|> p'otherdetail
         p'otherdetail = Others <$> AtT.takeWhile (AtT.notInClass "\n") <* AtT.char '\n'
         p'delivery = (\i j l _ -> Delivery (Map.fromList ([("umsgid",i), ("append", l)] ++ j))) <$>
                      p'msgid <* AtT.char ':' <*> AtT.sepBy p'dafields (AtT.char ',') <*> AtT.takeWhile (AtT.notInClass "\n")
                      <*> AtT.option Nothing (Just <$> AtT.takeWhile (/= '\n'))
                      <* AtT.char '\n'
         p'msgid = AtT.takeWhile (AtT.inClass "-a-zA-Z0-9+_.")
         p'dafields = (,) <$> AtT.takeWhile (AtT.notInClass ", =\n") <* AtT.char '=' <*> AtT.takeWhile (AtT.notInClass ",\n")





-- Test

-- _isDelivery (Delivery _) = True
-- _isDelivery _ = False
-- _deliveryFields (Delivery x) = x
-- _deliveryFields _ = error "not a delivery detail"
-- _getDeliField k (Delivery x) = Map.lookup k x
-- _getDeliField _ _ = error "not a delivery detail"
