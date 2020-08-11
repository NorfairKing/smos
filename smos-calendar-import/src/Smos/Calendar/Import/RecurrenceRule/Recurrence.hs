{-# LANGUAGE RecordWildCards #-}

-- This module uses list as a monad a lot, make sure you understand it before reading this module.
module Smos.Calendar.Import.RecurrenceRule.Recurrence where

import Control.Monad
import Data.Fixed
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time
import Data.Time.Calendar.MonthDay
import Data.Validity.Containers ()
import Data.Validity.Time ()
import Safe
import Smos.Calendar.Import.RecurrenceRule.Recurrence.Util
import Smos.Calendar.Import.RecurrenceRule.Type
import Smos.Calendar.Import.WeekDate

-- Recurrence rules operate on LocalTime instead of CalDateTime because of this line in the spec:
--
-- The "DTSTART" property specified within the iCalendar object defines the
-- first instance of the recurrence.  In most cases, a "DTSTART" property of
-- DATE-TIME value type used with a recurrence rule, should be specified as a
-- date with local time and time zone reference to make sure all the recurrence
-- instances start at the same local time regardless of time zone changes.
--
-- This function takes care of the 'rRuleUntilCount' part.
rruleDateTimeOccurrencesUntil ::
  -- | DTStart
  LocalTime ->
  -- | recurrence rule
  RRule ->
  -- | Limit
  LocalTime ->
  -- | The recurrence set.
  -- For infinte recurrence sets, these are only the occurrences before (inclusive) the limit.
  Set LocalTime
rruleDateTimeOccurrencesUntil = occurrencesUntil rruleDateTimeNextOccurrence (<=)

rruleDateOccurrencesUntil ::
  -- | DTStart
  Day ->
  -- | recurrence rule
  RRule ->
  -- | Limit
  Day ->
  -- | The recurrence set.
  -- For infinte recurrence sets, these are only the occurrences before (inclusive) the limit.
  Set Day
rruleDateOccurrencesUntil = occurrencesUntil rruleDateNextOccurrence (\d lt -> d <= localDay lt)

occurrencesUntil :: Ord a => (a -> a -> RRule -> Maybe a) -> (a -> LocalTime -> Bool) -> a -> RRule -> a -> Set a
occurrencesUntil func leFunc start rrule limit = case rRuleUntilCount rrule of
  Indefinitely -> goIndefinitely
  Count i -> goCount i
  Until lt -> goUntil lt
  where
    goUntil untilLimit = S.filter (`leFunc` untilLimit) goIndefinitely
    goCount count = S.take (fromIntegral count) goIndefinitely
    goIndefinitely = iterateMaybeSet (\cur -> func cur limit rrule) start

-- This function takes care of the 'rRuleFrequency' part.
rruleDateTimeNextOccurrence :: LocalTime -> LocalTime -> RRule -> Maybe LocalTime
rruleDateTimeNextOccurrence lt limit RRule {..} = case rRuleFrequency of
  -- 1. From the spec:
  --
  --    > The BYDAY rule part MUST NOT be specified with a numeric value when
  --    > the FREQ rule part is not set to MONTHLY or YEARLY.  Furthermore,
  --
  --    So we 'filterEvery' on the 'byDay's for every frequency except 'MONTHLY' and 'YEARLY'.
  Daily -> dailyDateTimeNextRecurrence lt limit rRuleInterval rRuleByMonth rRuleByMonthDay (filterEvery rRuleByDay) rRuleByHour rRuleByMinute rRuleBySecond rRuleBySetPos
  Weekly -> weeklyDateTimeNextRecurrence lt limit rRuleInterval rRuleByMonth rRuleWeekStart (filterEvery rRuleByDay) rRuleByHour rRuleByMinute rRuleBySecond rRuleBySetPos
  Monthly -> monthlyDateTimeNextRecurrence lt limit rRuleInterval rRuleByMonth rRuleByMonthDay rRuleByDay rRuleByHour rRuleByMinute rRuleBySecond rRuleBySetPos
  _ -> Nothing

rruleDateNextOccurrence :: Day -> Day -> RRule -> Maybe Day
rruleDateNextOccurrence d limit RRule {..} =
  case rRuleFrequency of
    -- 1. From the spec:
    --
    --    > The BYDAY rule part MUST NOT be specified with a numeric value when
    --    > the FREQ rule part is not set to MONTHLY or YEARLY.  Furthermore,
    --
    --    So we 'filterEvery' on the 'byDay's for every frequency except 'MONTHLY' and 'YEARLY'.
    --
    -- 2. By set pos is ignored because every day is the only day in a daily interval
    Daily -> dailyDateNextRecurrence d limit rRuleInterval rRuleByMonth rRuleByMonthDay (filterEvery rRuleByDay) -- By set pos is ignored because every day is the only day in a daily interval
    Weekly -> weeklyDateNextRecurrence d limit rRuleInterval rRuleByMonth rRuleWeekStart (filterEvery rRuleByDay) rRuleBySetPos
    Monthly -> monthlyDateNextRecurrence d limit rRuleInterval rRuleByMonth rRuleByMonthDay rRuleByDay rRuleBySetPos
    _ -> Nothing

-- | Recur with a 'Daily' frequency
dailyDateTimeNextRecurrence ::
  LocalTime ->
  LocalTime ->
  Interval ->
  Set ByMonth ->
  Set ByMonthDay ->
  Set DayOfWeek ->
  Set ByHour ->
  Set ByMinute ->
  Set BySecond ->
  Set BySetPos ->
  Maybe LocalTime
dailyDateTimeNextRecurrence
  lt@(LocalTime d_ tod_)
  limit@(LocalTime limitDay _)
  interval
  byMonths
  byMonthDays
  byDays
  byHours
  byMinutes
  bySeconds
  bySetPoss = headMay $ do
    d <- dailyDayRecurrence d_ limitDay interval byMonths byMonthDays byDays
    tod <- filterSetPos bySetPoss $ timeOfDayExpand tod_ byHours byMinutes bySeconds
    let next = LocalTime d tod
    guard (next > lt) -- Don't take the current one again
    guard (next <= limit) -- Don't go beyond the limit
    pure next

-- | Recur with a 'Daily' frequency
dailyDateNextRecurrence ::
  Day ->
  Day ->
  Interval ->
  Set ByMonth ->
  Set ByMonthDay ->
  Set DayOfWeek ->
  Maybe Day
dailyDateNextRecurrence
  d_
  limitDay
  interval
  byMonths
  byMonthDays
  byDays =
    headMay $ do
      d <- dailyDayRecurrence d_ limitDay interval byMonths byMonthDays byDays
      guard (d > d_) -- Don't take the current one again
      guard (d <= limitDay) -- Don't go beyond the limit
      pure d

-- | Internal: Get all the relevant days until the limit, not considering any 'Set BySetPos'
dailyDayRecurrence ::
  Day ->
  Day ->
  Interval ->
  Set Month ->
  Set ByMonthDay ->
  Set DayOfWeek ->
  [Day]
dailyDayRecurrence
  d_
  limitDay
  (Interval interval)
  byMonths
  byMonthDays
  byDays = do
    d <- takeWhile (<= limitDay) $ map (\i -> addDays (fromIntegral interval * i) d_) [0 ..]
    guard $ byMonthLimit byMonths d
    guard $ byMonthDayLimit byMonthDays d
    guard $ byEveryWeekDayLimit byDays d
    pure d

-- | Recur with a 'Weekly' frequency
weeklyDateTimeNextRecurrence ::
  LocalTime ->
  LocalTime ->
  Interval ->
  Set ByMonth ->
  DayOfWeek ->
  Set DayOfWeek ->
  Set ByHour ->
  Set ByMinute ->
  Set BySecond ->
  Set BySetPos ->
  Maybe LocalTime
weeklyDateTimeNextRecurrence
  lt@(LocalTime d_ tod_)
  limit@(LocalTime limitDay _)
  interval
  byMonths
  weekStart
  byDays
  byHours
  byMinutes
  bySeconds
  bySetPoss =
    headMay $ do
      d <- weeklyDayRecurrence d_ limitDay interval byMonths weekStart byDays bySetPoss
      tod <- timeOfDayExpand tod_ byHours byMinutes bySeconds
      let next = LocalTime d tod
      guard (next > lt) -- Don't take the current one again
      guard (next <= limit) -- Don't go beyond the limit
      pure next

-- | Recur with a 'Weekly' frequency
weeklyDateNextRecurrence ::
  Day ->
  Day ->
  Interval ->
  Set ByMonth ->
  DayOfWeek ->
  Set DayOfWeek ->
  Set BySetPos ->
  Maybe Day
weeklyDateNextRecurrence
  d_
  limitDay
  interval
  byMonths
  weekStart
  byDays
  bySetPoss =
    headMay $ do
      d <- weeklyDayRecurrence d_ limitDay interval byMonths weekStart byDays bySetPoss
      guard (d > d_) -- Don't take the current one again
      pure d

-- | Internal: Get all the relevant days until the limit, not considering any 'Set BySetPos'
weeklyDayRecurrence ::
  Day ->
  Day ->
  Interval ->
  Set Month ->
  DayOfWeek ->
  Set DayOfWeek ->
  Set BySetPos ->
  [Day]
weeklyDayRecurrence
  d_
  limitDay
  (Interval interval)
  byMonths
  weekStart
  byDays
  bySetPoss = do
    let (y, w, dow) = toWeekDateWithStart weekStart d_
    let (limitY, limitWN, _) = toWeekDateWithStart weekStart limitDay
    (y', w') <-
      takeEvery interval
        $ takeWhile (<= (limitY, limitWN))
        $ weeksIntoTheFutureStartingFrom weekStart y w
    d <-
      filterSetPos bySetPoss
        $ sort -- Need to sort because the week days may not be in order.
        $ if S.null byDays
          then maybeToList $ fromWeekDateWithStart weekStart y' w' dow
          else do
            dow' <- S.toList byDays
            maybeToList $ fromWeekDateWithStart weekStart y' w' dow'
    guard $ byMonthLimit byMonths d
    guard (d <= limitDay) -- Don't go beyond the limit
    pure d

monthlyDateTimeNextRecurrence ::
  LocalTime ->
  LocalTime ->
  Interval ->
  Set ByMonth ->
  Set ByMonthDay ->
  Set ByDay ->
  Set ByHour ->
  Set ByMinute ->
  Set BySecond ->
  Set BySetPos ->
  Maybe LocalTime
monthlyDateTimeNextRecurrence
  lt@(LocalTime d_ tod_)
  limit@(LocalTime limitDay _)
  interval
  byMonths
  byMonthDays
  byDays
  byHours
  byMinutes
  bySeconds
  bySetPoss =
    headMay $ do
      d <- monthlyDayRecurrence d_ limitDay interval byMonths byMonthDays byDays bySetPoss
      tod <- timeOfDayExpand tod_ byHours byMinutes bySeconds
      let next = LocalTime d tod
      guard (next > lt) -- Don't take the current one again
      guard (next <= limit) -- Don't go beyond the limit
      pure next

monthlyDateNextRecurrence ::
  Day ->
  Day ->
  Interval ->
  Set ByMonth ->
  Set ByMonthDay ->
  Set ByDay ->
  Set BySetPos ->
  Maybe Day
monthlyDateNextRecurrence
  d_
  limitDay
  interval
  byMonths
  byMonthDays
  byDays
  bySetPoss =
    headMay $ do
      d <- monthlyDayRecurrence d_ limitDay interval byMonths byMonthDays byDays bySetPoss
      guard (d > d_) -- Don't take the current one again
      pure d

monthlyDayRecurrence ::
  Day ->
  Day ->
  Interval ->
  Set ByMonth ->
  Set ByMonthDay ->
  Set ByDay ->
  Set BySetPos ->
  [Day]
monthlyDayRecurrence
  d_
  limitDay
  (Interval interval)
  byMonths
  byMonthDays
  byDays
  bySetPoss = do
    let (year_, month_, md_) = toGregorian d_
    let (limitYear, limitMonth, _) = toGregorian limitDay
    (year, month) <- takeEvery interval $ takeWhile (<= (limitYear, limitMonth)) $ dropWhile (< (year_, month_)) $ do
      y <- [year_ .. limitYear]
      m <- [1 .. 12]
      pure (y, m)
    d <- filterSetPos bySetPoss $ do
      m <- maybeToList $ monthNoToMonth month
      guard $ byMonthLimitMonth byMonths m
      md <- byMonthDayExpand year m md_ byMonthDays
      d <- maybeToList $ fromGregorianValid year month md
      if S.null byMonthDays
        then do
          guard $ byDayLimit byDays d
          pure d
        else byDayExand d byDays
    guard (d <= limitDay) -- Don't go beyond the limit
    pure d
