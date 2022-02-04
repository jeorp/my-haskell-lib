{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Clock.System
import Data.Time.Calendar.OrdinalDate

dayToString :: Day -> String
dayToString = filter (/= '-') . show

getToday :: IO Day
getToday = getCurrentTime >>= fmap localDay . utcToLocal

getNmonthAgo :: Int -> IO Day
getNmonthAgo n = do
  today <- getToday
  let (year, day) = toOrdinalDate today
      nMonthAgo = day - n*30
   in loop year nMonthAgo 
  where
    loop year ago 
     | ago >= 0 = return $ fromOrdinalDate year ago
     | otherwise = do
       let days = if (year - 1) `mod` 4 == 0 then 366 else 365
         in loop (year - 1) (days + ago)

utcToLocal :: UTCTime -> IO LocalTime 
utcToLocal = fmap zonedTimeToLocalTime . utcToLocalZonedTime

sysToLocal :: SystemTime -> IO LocalTime 
sysToLocal =  utcToLocal . systemToUTCTime


tokyoTimeZone :: TimeZone
tokyoTimeZone = minutesToTimeZone 540

utcToTokyoTime :: UTCTime -> ZonedTime
utcToTokyoTime = utcToZonedTime tokyoTimeZone