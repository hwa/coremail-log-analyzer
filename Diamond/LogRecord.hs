module Diamond.LogRecord
       (LogRecord(..))
       where

import qualified Diamond.Parser.DALog as DA
import qualified Diamond.Parser.MTALog as MTA
import           Diamond.Utils.DateTime


class LogRecord a where
  -- | hour, minute, and secord of the record
  recordTime :: a -> [Int]


instance LogRecord DA.LogRecord where
  recordTime (DA.LogRecord time _ _) = time

instance LogRecord MTA.LogRecord where
  recordTime (MTA.LogRecord time _) = time

-- | given (year, month, day), calculate unix time of the record
dating :: (LogRecord a) => (Int, Int, Int) -> a -> (a, Int)
dating (year, month, day) logrecord =
  (logrecord, toUnixSeconds (year, month,  day) (hour, minute, second))
  where
    [hour, minute, second] = recordTime logrecord
