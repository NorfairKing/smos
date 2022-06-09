{-# LANGUAGE RecordWildCards #-}

-- This module uses list as a monad a lot, make sure you understand it before reading this module.
module Smos.Calendar.Import.RecurrenceRule.Recurrence where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Time
import Safe
import Smos.Calendar.Import.RecurrenceRule.Recurrence.Daily
import Smos.Calendar.Import.RecurrenceRule.Recurrence.Monthly
import Smos.Calendar.Import.RecurrenceRule.Recurrence.Util
import Smos.Calendar.Import.RecurrenceRule.Recurrence.Weekly
import Smos.Calendar.Import.RecurrenceRule.Recurrence.Yearly
import Smos.Calendar.Import.RecurrenceRule.Type

rruleSetDateTimeOccurrencesUntil ::
  -- | DTStart
  LocalTime ->
  -- | recurrence rule
  Set RRule ->
  -- | Limit
  LocalTime ->
  -- | The recurrence set.
  -- For infinte recurrence sets, these are only the occurrences before (inclusive) the limit.
  Set LocalTime
rruleSetDateTimeOccurrencesUntil lt rules limit =
  S.unions $ map (\rule -> rruleDateTimeOccurrencesUntil lt rule limit) $ S.toList rules

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
rruleDateTimeOccurrencesUntil = occurrencesUntil rruleDateTimeOccurrence (<=)

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
rruleDateOccurrencesUntil = occurrencesUntil rruleDateOccurrence (\d lt -> d <= localDay lt)

occurrencesUntil :: Ord a => (a -> a -> RRule -> [a]) -> (a -> LocalTime -> Bool) -> a -> RRule -> a -> Set a
occurrencesUntil func leFunc start rrule limit = S.insert start $ case rRuleUntilCount rrule of
  Indefinitely -> goIndefinitely
  Count i -> goCount (i - 1)
  Until lt -> goUntil lt
  where
    goUntil untilLimit = occurUntil untilLimit $ func start limit rrule
    occurUntil _ [] = S.empty
    occurUntil untilLimit (l : ls) =
      if l `leFunc` untilLimit
        then S.insert l $ occurUntil untilLimit ls
        else S.empty
    goCount c = occurCount c $ func start limit rrule
    occurCount _ [] = S.empty
    occurCount 0 _ = S.empty
    occurCount c (a : as) = S.insert a $ occurCount (pred c) as
    goIndefinitely = iterateMaybeSet (\cur -> headMay $ func cur limit rrule) start

-- This function takes care of the 'rRuleFrequency' part.
rruleDateTimeOccurrence :: LocalTime -> LocalTime -> RRule -> [LocalTime]
rruleDateTimeOccurrence lt limit RRule {..} = case rRuleFrequency of
  -- 1. From the spec:
  --
  --    > The BYDAY rule part MUST NOT be specified with a numeric value when
  --    > the FREQ rule part is not set to MONTHLY or YEARLY.  Furthermore,
  --
  --    So we 'filterEvery' on the 'byDay's for every frequency except 'MONTHLY' and 'YEARLY'.
  Daily -> dailyDateTimeRecurrence lt limit rRuleInterval rRuleByMonth rRuleByMonthDay (filterEvery rRuleByDay) rRuleByHour rRuleByMinute rRuleBySecond rRuleBySetPos
  Weekly -> weeklyDateTimeRecurrence lt limit rRuleInterval rRuleByMonth rRuleWeekStart (filterEvery rRuleByDay) rRuleByHour rRuleByMinute rRuleBySecond rRuleBySetPos
  Monthly -> monthlyDateTimeRecurrence lt limit rRuleInterval rRuleByMonth rRuleByMonthDay rRuleByDay rRuleByHour rRuleByMinute rRuleBySecond rRuleBySetPos
  Yearly -> yearlyDateTimeRecurrence lt limit rRuleInterval rRuleByMonth rRuleWeekStart rRuleByWeekNo rRuleByYearDay rRuleByMonthDay rRuleByDay rRuleByHour rRuleByMinute rRuleBySecond rRuleBySetPos
  _ -> error $ "not implemented yet: " <> show rRuleFrequency

rruleDateOccurrence :: Day -> Day -> RRule -> [Day]
rruleDateOccurrence d limit RRule {..} =
  case rRuleFrequency of
    -- 1. From the spec:
    --
    --    > The BYDAY rule part MUST NOT be specified with a numeric value when
    --    > the FREQ rule part is not set to MONTHLY or YEARLY.  Furthermore,
    --
    --    So we 'filterEvery' on the 'byDay's for every frequency except 'MONTHLY' and 'YEARLY'.
    --
    -- 2. By set pos is ignored because every day is the only day in a daily interval
    Daily -> dailyDateRecurrence d limit rRuleInterval rRuleByMonth rRuleByMonthDay (filterEvery rRuleByDay) -- By set pos is ignored because every day is the only day in a daily interval
    Weekly -> weeklyDateRecurrence d limit rRuleInterval rRuleByMonth rRuleWeekStart (filterEvery rRuleByDay) rRuleBySetPos
    Monthly -> monthlyDateRecurrence d limit rRuleInterval rRuleByMonth rRuleByMonthDay rRuleByDay rRuleBySetPos
    Yearly -> yearlyDateRecurrence d limit rRuleInterval rRuleByMonth rRuleWeekStart rRuleByWeekNo rRuleByYearDay rRuleByMonthDay rRuleByDay rRuleBySetPos
    _ -> error $ "not implemented yet: " <> show rRuleFrequency
