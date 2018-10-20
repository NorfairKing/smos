{-# LANGUAGE DeriveGeneric #-}

module Smos.Report.Period where

import GHC.Generics (Generic)

import Data.Validity

import Data.Time
import Data.Time.Calendar.WeekDate

data Period
    = Today
    | ThisWeek
    | AllTime
    deriving (Show, Eq, Generic)

instance Validity Period

filterPeriod :: ZonedTime -> Period -> UTCTime -> Bool
filterPeriod now p u =
    (case p of
         AllTime -> const True
         Today -> filterBetween todayStart todayEnd
         ThisWeek -> filterBetween thisWeekStart thisWeekEnd) $
    utcToLocalTime tz u
  where
    tz :: TimeZone
    tz = zonedTimeZone now
    nowLocal :: LocalTime
    nowLocal = zonedTimeToLocalTime now
    today :: Day
    today = localDay nowLocal
    filterBetween :: LocalTime -> LocalTime -> LocalTime -> Bool
    filterBetween start end lt = start <= lt && lt < end
    todayStart :: LocalTime
    todayStart = nowLocal {localTimeOfDay = midnight}
    todayEnd :: LocalTime
    todayEnd = nowLocal {localDay = addDays 1 today, localTimeOfDay = midnight}
    thisWeekStart :: LocalTime
    thisWeekStart =
        let (y, wn, _) = toWeekDate today
         in LocalTime (fromWeekDate y wn 1) midnight
    thisWeekEnd :: LocalTime
    thisWeekEnd =
        let (y, wn, _) = toWeekDate today
         in LocalTime (fromWeekDate y (wn + 1) 1) midnight -- FIXME this can wrong at the end of the year
