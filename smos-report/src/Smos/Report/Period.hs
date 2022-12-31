{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- TODO: Most of this module can be refactored out as soon as we can use
-- Data.Time.Calendar's 'DayPeriod' functions.
module Smos.Report.Period where

import Data.Time
import Data.Time.Calendar.WeekDate
import Data.Time.Zones
import Data.Validity
import Data.Validity.Time ()
import GHC.Generics (Generic)
import Smos.Data
import Smos.Report.TimeBlock

-- | A time Period, as specified by a human
data Period
  = Yesterday
  | Today
  | Tomorrow
  | LastWeek
  | -- | The 7 days that end in today
    PastWeek
  | ThisWeek
  | -- | The 7 days that start today
    ComingWeek
  | NextWeek
  | LastMonth
  | -- | The 30 days that end in today
    PastMonth
  | ThisMonth
  | -- | The 30 days that start today
    ComingMonth
  | NextMonth
  | LastYear
  | -- | The 365 days that end in today
    PastYear
  | ThisYear
  | -- | The 365 days that start today
    ComingYear
  | NextYear
  | AllTime
  | BeginOnly !Day
  | EndOnly !Day
  | BeginEnd !Day !Day -- If end is before begin, this matches nothing
  deriving (Show, Eq, Generic)

instance Validity Period

periodInterval :: Day -> Period -> Interval
periodInterval today =
  \case
    Yesterday -> dayInterval $ addDays (-1) today
    Today -> dayInterval today
    Tomorrow -> dayInterval $ addDays 1 today
    LastWeek ->
      weekInterval $
        let (y, w, _) = toWeekDate today
         in if w == 1 then WeekNumber (pred y) 53 else WeekNumber y (pred w)
    PastWeek -> Interval (addDays (-7) today) today
    ThisWeek ->
      weekInterval $
        let (y, w, _) = toWeekDate today
         in WeekNumber y w
    ComingWeek -> Interval today (addDays 7 today)
    NextWeek ->
      weekInterval $
        let (y, w, _) = toWeekDate today
         in if w >= 52 then WeekNumber (succ y) 1 else WeekNumber y (succ w)
    PastMonth -> Interval (addDays (-30) today) today
    LastMonth ->
      monthInterval $
        let (y, m, _) = toGregorian today
         in if m == 1 then MonthNumber (pred y) 12 else MonthNumber y (pred m)
    ComingMonth -> Interval today (addDays 30 today)
    ThisMonth ->
      monthInterval $
        let (y, m, _) = toGregorian today
         in MonthNumber y m
    NextMonth ->
      monthInterval $
        let (y, m, _) = toGregorian today
         in if m >= 12 then MonthNumber (succ y) 1 else MonthNumber y (succ m)
    LastYear -> yearInterval $ succ $ dayYear today
    PastYear -> Interval (addDays (-365) today) today
    ThisYear -> yearInterval $ dayYear today
    ComingYear -> Interval today (addDays 365 today)
    NextYear -> yearInterval $ succ $ dayYear today
    AllTime -> EverythingInterval
    BeginOnly begin -> BeginOnlyInterval begin
    EndOnly end -> EndOnlyInterval end
    BeginEnd begin end -> Interval begin end

-- | An interval of time.
data Interval
  = -- | Unbounded interval
    EverythingInterval
  | -- | An interval that's only bounded on the begin side
    BeginOnlyInterval !Day
  | -- | An interval that's only bounded on the end side
    EndOnlyInterval !Day
  | -- | Interval bounded on both sides
    -- It is half open, open on the end.
    -- I.e. Interval a b represents [a .. b)
    Interval !Day !Day
  deriving (Show, Eq, Generic)

instance Validity Interval

filterIntervalUTCTime :: TZ -> Interval -> UTCTime -> Bool
filterIntervalUTCTime zone interval ut = filterIntervalLocalTime interval $ utcToLocalTimeTZ zone ut

filterIntervalTimestamp :: Interval -> Timestamp -> Bool
filterIntervalTimestamp interval timestamp = filterIntervalDay interval $ timestampDay timestamp

filterIntervalLocalTime :: Interval -> LocalTime -> Bool
filterIntervalLocalTime interval lt = filterIntervalDay interval (localDay lt)

-- | Check whether a given 'Interval' contains a given 'Day'
filterIntervalDay :: Interval -> Day -> Bool
filterIntervalDay i d = case i of
  EverythingInterval -> True
  BeginOnlyInterval begin -> begin <= d
  EndOnlyInterval end -> d < end
  Interval begin end -> begin <= d && d < end

intervalTuple :: Interval -> (Maybe Day, Maybe Day)
intervalTuple = \case
  EverythingInterval -> (Nothing, Nothing)
  BeginOnlyInterval begin -> (Just begin, Nothing)
  EndOnlyInterval end -> (Nothing, Just end)
  Interval begin end -> (Just begin, Just end)

-- An 'Interval' representing the given 'Year'
yearInterval :: YearNumber -> Interval
yearInterval y = Interval yearStart nextYearStart
  where
    yearStart :: Day
    yearStart = fromGregorian y 1 1
    nextYearStart :: Day
    nextYearStart = fromGregorian (succ y) 1 1

-- An 'Interval' representing the given 'Month'
monthInterval :: MonthNumber -> Interval
monthInterval MonthNumber {..} = Interval monthStart nextMonthStart
  where
    monthStart :: Day
    monthStart = fromGregorian monthNumberYear monthNumberMonth 1
    nextMonthStart :: Day
    nextMonthStart =
      if monthNumberMonth == 12
        then fromGregorian (succ monthNumberYear) 1 1
        else fromGregorian monthNumberYear (succ monthNumberMonth) 1

-- An 'Interval' representing the given 'Week'
weekInterval :: WeekNumber -> Interval
weekInterval WeekNumber {..} = Interval weekStart nextWeekStart
  where
    weekStart :: Day
    weekStart = fromWeekDate weekNumberYear weekNumberWeek 1
    nextWeekStart :: Day
    nextWeekStart =
      case fromWeekDateValid weekNumberYear (weekNumberWeek + 1) 1 of
        Nothing -> fromWeekDate (succ weekNumberYear) 1 1
        Just d -> d

-- An 'Interval' representing the given 'Day'
dayInterval :: Day -> Interval
dayInterval d = Interval d (addDays 1 d)
