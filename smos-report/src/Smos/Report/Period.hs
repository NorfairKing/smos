{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Period where

import GHC.Generics (Generic)

import Data.Validity
import Data.Validity.Time ()

import Data.Time
import Data.Time.Calendar.WeekDate

import Smos.Report.TimeBlock

data Period
  = Today
  | Yesterday
  | ThisWeek
  | LastWeek
  | ThisMonth
  | LastMonth -- TODO add year
  | ThisYear
  | LastYear
  | AllTime
  | BeginOnly LocalTime
  | EndOnly LocalTime
  | BeginEnd LocalTime LocalTime -- If end is before begin, this matches nothing
  deriving (Show, Eq, Generic)

instance Validity Period

yearPeriod :: YearNumber -> Period
yearPeriod y = BeginEnd monthStart monthEnd
  where
    monthStart :: LocalTime
    monthStart = LocalTime (fromGregorian y 1 1) midnight
    monthEnd :: LocalTime
    monthEnd = LocalTime (fromGregorian (y + 1) 1 1) midnight

monthPeriod :: MonthNumber -> Period
monthPeriod MonthNumber {..} = BeginEnd monthStart monthEnd
  where
    monthStart :: LocalTime
    monthStart = LocalTime (fromGregorian monthNumberYear monthNumberMonth 1) midnight
    monthEnd :: LocalTime
    monthEnd =
      LocalTime (fromGregorian monthNumberYear (monthNumberMonth + 1) 1) midnight -- FIXME this can wrong at the end of the year

weekPeriod :: WeekNumber -> Period
weekPeriod WeekNumber {..} = BeginEnd weekStart weekEnd
  where
    weekStart :: LocalTime
    weekStart = LocalTime (fromWeekDate weekNumberYear weekNumberWeek 1) midnight
    weekEnd :: LocalTime
    weekEnd =
      LocalTime (fromWeekDate weekNumberYear (weekNumberWeek + 1) 1) midnight -- FIXME this can wrong at the end of the year

dayPeriod :: Day -> Period
dayPeriod d = BeginEnd dayStart dayEnd
  where
    dayStart = LocalTime {localDay = d, localTimeOfDay = midnight}
    dayEnd = LocalTime {localDay = addDays 1 d, localTimeOfDay = midnight}

filterPeriod :: ZonedTime -> Period -> UTCTime -> Bool
filterPeriod now p u =
  (case p of
     AllTime -> const True
     Today -> filterBetween todayStart todayEnd
     Yesterday -> filterBetween yesterdayStart yesterdayEnd
     LastWeek -> filterBetween lastWeekStart thisWeekStart
     ThisWeek -> filterBetween thisWeekStart thisWeekEnd
     LastMonth -> filterBetween lastMonthStart thisMonthStart
     ThisMonth -> filterBetween thisMonthStart thisMonthEnd
     LastYear -> filterBetween lastYearStart thisYearStart
     ThisYear -> filterBetween thisYearStart thisYearEnd
     BeginOnly begin -> (begin <=)
     EndOnly end -> (< end)
     BeginEnd begin end -> filterBetween begin end) $
  utcToLocalTime tz u
  where
    tz :: TimeZone
    tz = zonedTimeZone now
    nowLocal :: LocalTime
    nowLocal = zonedTimeToLocalTime now
    today :: Day
    today = localDay nowLocal
    yesterday :: Day
    yesterday = pred today
    filterBetween :: LocalTime -> LocalTime -> LocalTime -> Bool
    filterBetween start end lt = start <= lt && lt < end
    todayStart :: LocalTime
    todayStart = nowLocal {localTimeOfDay = midnight}
    todayEnd :: LocalTime
    todayEnd = nowLocal {localDay = addDays 1 today, localTimeOfDay = midnight}
    yesterdayStart :: LocalTime
    yesterdayStart = LocalTime {localDay = yesterday, localTimeOfDay = midnight}
    yesterdayEnd :: LocalTime
    yesterdayEnd = LocalTime {localDay = today, localTimeOfDay = midnight}
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
    lastMonthStart :: LocalTime
    lastMonthStart =
      let (y, m, _) = toGregorian today
       in LocalTime (fromGregorian y (m - 1) 1) midnight -- This will fail around newyear
    thisMonthStart :: LocalTime
    thisMonthStart =
      let (y, m, _) = toGregorian today
       in LocalTime (fromGregorian y m 1) midnight
    thisMonthEnd :: LocalTime
    thisMonthEnd =
      let (y, m, _) = toGregorian today
       in LocalTime (fromGregorian y m 31) midnight
    lastYearStart :: LocalTime
    lastYearStart =
      let (y, _, _) = toGregorian today
       in LocalTime (fromGregorian (y - 1) 1 1) midnight -- This will fail around newyear
    thisYearStart :: LocalTime
    thisYearStart =
      let (y, _, _) = toGregorian today
       in LocalTime (fromGregorian y 1 1) midnight
    thisYearEnd :: LocalTime
    thisYearEnd =
      let (y, _, _) = toGregorian today
       in LocalTime (fromGregorian y 12 31) midnight
