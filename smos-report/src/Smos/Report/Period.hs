{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Period where

import Data.Time
import Data.Time.Calendar.WeekDate
import Data.Validity
import Data.Validity.Time ()
import GHC.Generics (Generic)
import Smos.Report.TimeBlock

data Period
  = Yesterday
  | Today
  | Tomorrow
  | LastWeek
  | ThisWeek
  | NextWeek
  | LastMonth -- TODO add year
  | ThisMonth
  | NextMonth
  | LastYear
  | ThisYear
  | NextYear
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

filterPeriodLocal :: ZonedTime -> Period -> LocalTime -> Bool
filterPeriodLocal now p l =
  ( case p of
      AllTime -> const True
      Yesterday -> filterBetween yesterdayStart yesterdayEnd
      Today -> filterBetween todayStart todayEnd
      Tomorrow -> filterBetween tomorrowStart tomorrowEnd
      LastWeek -> filterBetween lastWeekStart lastWeekEnd
      ThisWeek -> filterBetween thisWeekStart thisWeekEnd
      NextWeek -> filterBetween nextWeekStart nextWeekEnd
      LastMonth -> filterBetween lastMonthStart lastMonthEnd
      ThisMonth -> filterBetween thisMonthStart thisMonthEnd
      NextMonth -> filterBetween nextMonthStart nextMonthEnd
      LastYear -> filterBetween lastYearStart lastYearEnd
      ThisYear -> filterBetween thisYearStart thisYearEnd
      NextYear -> filterBetween nextYearStart nextYearEnd
      BeginOnly begin -> (begin <=)
      EndOnly end -> (< end)
      BeginEnd begin end -> filterBetween begin end
  )
    l
  where
    nowLocal :: LocalTime
    nowLocal = zonedTimeToLocalTime now
    today :: Day
    today = localDay nowLocal
    filterBetween :: LocalTime -> LocalTime -> LocalTime -> Bool
    filterBetween start end lt = start <= lt && lt < end
    yesterdayStart :: LocalTime
    yesterdayStart = LocalTime {localDay = addDays (-1) today, localTimeOfDay = midnight}
    yesterdayEnd :: LocalTime
    yesterdayEnd = todayStart
    todayStart :: LocalTime
    todayStart = LocalTime {localDay = today, localTimeOfDay = midnight}
    todayEnd :: LocalTime
    todayEnd = nowLocal {localDay = addDays 1 today, localTimeOfDay = midnight}
    tomorrowStart :: LocalTime
    tomorrowStart = todayEnd
    tomorrowEnd :: LocalTime
    tomorrowEnd = LocalTime {localDay = addDays 2 today, localTimeOfDay = midnight}
    lastWeekStart :: LocalTime
    lastWeekStart =
      let (y, wn, _) = toWeekDate today
       in LocalTime (fromWeekDate y (wn - 1) 1) midnight -- TODO this will fail around newyear
    lastWeekEnd :: LocalTime
    lastWeekEnd = thisWeekStart
    thisWeekStart :: LocalTime
    thisWeekStart =
      let (y, wn, _) = toWeekDate today
       in LocalTime (fromWeekDate y wn 1) midnight
    thisWeekEnd :: LocalTime
    thisWeekEnd =
      let (y, wn, _) = toWeekDate today
       in LocalTime (fromWeekDate y (wn + 1) 1) midnight -- FIXME this can wrong at the end of the year
    nextWeekStart :: LocalTime
    nextWeekStart = thisWeekEnd
    nextWeekEnd :: LocalTime
    nextWeekEnd =
      let (y, wn, _) = toWeekDate today
       in LocalTime (fromWeekDate y (wn + 2) 1) midnight -- FIXME this can wrong at the end of the year
    lastMonthStart :: LocalTime
    lastMonthStart =
      let (y, m, _) = toGregorian today
       in LocalTime (fromGregorian y (m - 1) 1) midnight -- FIXME This will fail around newyear
    lastMonthEnd :: LocalTime
    lastMonthEnd = thisMonthStart
    thisMonthStart :: LocalTime
    thisMonthStart =
      let (y, m, _) = toGregorian today
       in LocalTime (fromGregorian y m 1) midnight
    thisMonthEnd :: LocalTime
    thisMonthEnd =
      let (y, m, _) = toGregorian today
       in LocalTime (fromGregorian y m 31) midnight
    nextMonthStart :: LocalTime
    nextMonthStart = thisMonthEnd
    nextMonthEnd :: LocalTime
    nextMonthEnd =
      let (y, m, _) = toGregorian today
       in LocalTime (fromGregorian y (m + 1) 31) midnight -- FIXME This will fail around newyear
    lastYearStart :: LocalTime
    lastYearStart =
      let (y, _, _) = toGregorian today
       in LocalTime (fromGregorian (y - 1) 1 1) midnight -- FIXME This will fail around newyear
    lastYearEnd :: LocalTime
    lastYearEnd = thisYearEnd
    thisYearStart :: LocalTime
    thisYearStart =
      let (y, _, _) = toGregorian today
       in LocalTime (fromGregorian y 1 1) midnight
    thisYearEnd :: LocalTime
    thisYearEnd =
      let (y, _, _) = toGregorian today
       in LocalTime (fromGregorian y 12 31) midnight
    nextYearStart :: LocalTime
    nextYearStart = thisYearEnd
    nextYearEnd :: LocalTime
    nextYearEnd =
      let (y, _, _) = toGregorian today
       in LocalTime (fromGregorian (y + 1) 12 31) midnight -- FIXME this will fail around newyear

filterPeriod :: ZonedTime -> Period -> UTCTime -> Bool
filterPeriod now p u =
  let tz :: TimeZone
      tz = zonedTimeZone now
   in filterPeriodLocal now p $ utcToLocalTime tz u
