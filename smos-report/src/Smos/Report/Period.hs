{-# LANGUAGE DeriveGeneric #-}

module Smos.Report.Period where

import GHC.Generics (Generic)

import Data.Validity
import Data.Validity.Time ()

import Data.Time
import Data.Time.Calendar.WeekDate

data Period
  = Today
  | ThisWeek
  | LastWeek
  | AllTime
  | BeginEnd LocalTime LocalTime -- If end is before begin, this matches nothing
  deriving (Show, Eq, Generic)

instance Validity Period

filterPeriod :: ZonedTime -> Period -> UTCTime -> Bool
filterPeriod now p u =
  (case p of
     AllTime -> const True
     Today -> filterBetween todayStart todayEnd
     LastWeek -> filterBetween lastWeekStart thisWeekStart
     ThisWeek -> filterBetween thisWeekStart thisWeekEnd
     BeginEnd begin end -> filterBetween begin end) $
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
    lastWeekStart :: LocalTime
    lastWeekStart =
      let (y, wn, _) = toWeekDate today
       in LocalTime (fromWeekDate y (wn - 1) 1) midnight -- TODO this will fail around newyear
    thisWeekStart :: LocalTime
    thisWeekStart =
      let (y, wn, _) = toWeekDate today
       in LocalTime (fromWeekDate y wn 1) midnight
    thisWeekEnd :: LocalTime
    thisWeekEnd =
      let (y, wn, _) = toWeekDate today
       in LocalTime (fromWeekDate y (wn + 1) 1) midnight -- FIXME this can wrong at the end of the year
