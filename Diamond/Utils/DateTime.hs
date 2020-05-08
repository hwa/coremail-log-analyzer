module Diamond.Utils.DateTime
       (currentTime,
        toUnixSeconds)
       where

import           Data.Time
import           Data.Time.Clock.POSIX
import           Control.Monad (liftM)
--import           Foreign.C.Types (CTime)

--unixEpoch :: UTCTime
--unixEpoch = UTCTime (fromGregorian 1970 1 1) 0

-- toUnixSeconds' :: UTCTime -> Int
-- toUnixSeconds' t = fromEnum $ diffUTCTime t unixEpoch / 10^12




--currentUnixSecond :: IO Int
--currentUnixSecond = fmap toUnixSeconds' getCurrentTime

currentTime :: IO Int
currentTime =
  let conv = fromEnum . realToFrac . utcTimeToPOSIXSeconds
  in
   liftM conv getCurrentTime

-- toUnixSeconds :: (Int, Int, Int) -> (Int, Int, Int) -> Int
-- toUnixSeconds (year, month, day) (hour, minute, second) =  -- Beijing date and time
--   let date = fromGregorian (toInteger year) month day
--       td = timeToTimeOfDay $ secondsToDiffTime $ toInteger (hour*3600 + minute*60 + second)
--       beijingZone = hoursToTimeZone 8
--       the'day = localTimeToUTC beijingZone $ LocalTime date td
--   in
--    toUnixSeconds' the'day

toUnixSeconds :: (Int, Int, Int) -> (Int, Int, Int) -> Int
toUnixSeconds (y, mon, d) (h, minu, s) =
  let day0 = fromGregorian 1970 1 1
      day1 = fromGregorian (toInteger y) mon d
      seconds = h * 3600 + minu * 60 + s
  in
   fromInteger (diffDays day1 day0) * 24 * 3600 + seconds
