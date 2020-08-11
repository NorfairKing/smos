-- This module uses list as a monad a lot, make sure you understand it before reading this module.
module Smos.Calendar.Import.RecurrenceRule.Recurrence.Daily where

import Control.Monad
import Data.Set (Set)
import Data.Time
import Safe
import Smos.Calendar.Import.RecurrenceRule.Recurrence.Util
import Smos.Calendar.Import.RecurrenceRule.Type

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
