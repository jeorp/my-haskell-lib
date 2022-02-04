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

getNdaysAgo :: Int -> IO Day
getNdaysAgo n = do
  today <- getToday
  let (year, day) = toOrdinalDate today
      nMonthAgo = day - n
   in loop year nMonthAgo 
  where
    daysOfYear year = if year `mod` 4 == 0 then 366 else 365
    loop year ago 
     | ago >= 0 = 
       let days = daysOfYear year
       in if ago > days
            then loop (year + 1) (ago - days) -- be able to input negate, ex getNdaysAgo (negate 365) == a year latter from today ! 
            else return $ fromOrdinalDate year ago
     | otherwise = 
       let days = daysOfYear (year - 1)
         in loop (year - 1) (days + ago)

getAboutNmonthAgo :: Int -> IO Day
getAboutNmonthAgo = getNdaysAgo . (*30)

utcToLocal :: UTCTime -> IO LocalTime 
utcToLocal = fmap zonedTimeToLocalTime . utcToLocalZonedTime

sysToLocal :: SystemTime -> IO LocalTime 
sysToLocal =  utcToLocal . systemToUTCTime


tokyoTimeZone :: TimeZone
tokyoTimeZone = minutesToTimeZone 540

utcToTokyoTime :: UTCTime -> ZonedTime
utcToTokyoTime = utcToZonedTime tokyoTimeZone