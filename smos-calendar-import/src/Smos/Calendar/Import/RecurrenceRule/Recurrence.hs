{-# LANGUAGE RecordWildCards #-}

-- This module uses list as a monad a lot, make sure you understand it before reading this module.
module Smos.Calendar.Import.RecurrenceRule.Recurrence where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Time
import Smos.Calendar.Import.RecurrenceRule.Recurrence.Daily
import Smos.Calendar.Import.RecurrenceRule.Recurrence.Monthly
import Smos.Calendar.Import.RecurrenceRule.Recurrence.Util
import Smos.Calendar.Import.RecurrenceRule.Recurrence.Weekly
import Smos.Calendar.Import.RecurrenceRule.Type

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
    goCount c = S.take (fromIntegral c) goIndefinitely
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
