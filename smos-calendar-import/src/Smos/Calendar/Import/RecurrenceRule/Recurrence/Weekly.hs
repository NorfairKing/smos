-- This module uses list as a monad a lot, make sure you understand it before reading this module.
module Smos.Calendar.Import.RecurrenceRule.Recurrence.Weekly where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Set (Set)
import Data.Time
import Safe
import Smos.Calendar.Import.RecurrenceRule.Recurrence.Util
import Smos.Calendar.Import.RecurrenceRule.Type
import Smos.Calendar.Import.WeekDate

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
      (y, w) <-
        weeklyWeekRecurrence
          d_
          limitDay
          interval
          weekStart
      next <- filterSetPos bySetPoss $ sort $ do
        -- Need to sort because week days may not be in order.
        dow <- byEveryWeekDayExpand (dayOfWeek d_) byDays
        d <- maybeToList $ fromWeekDateWithStart weekStart y w dow
        guard $ byMonthLimit byMonths d
        tod <- timeOfDayExpand tod_ byHours byMinutes bySeconds
        let next = LocalTime d tod
        pure next
      guard (next <= limit) -- Don't go beyond the limit
      guard (next > lt) -- Don't take the current one again
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
      (y, w) <-
        weeklyWeekRecurrence
          d_
          limitDay
          interval
          weekStart
      d <- filterSetPos bySetPoss $ sort $ do
        -- Need to sort because week days may not be in order.
        dow <- byEveryWeekDayExpand (dayOfWeek d_) byDays
        d <- maybeToList $ fromWeekDateWithStart weekStart y w dow
        guard $ byMonthLimit byMonths d
        pure d
      guard (d <= limitDay)
      guard (d > d_) -- Don't take the current one again
      pure d

weeklyWeekRecurrence ::
  Day ->
  Day ->
  Interval ->
  DayOfWeek ->
  [(Integer, Word)]
weeklyWeekRecurrence
  d_
  limitDay
  (Interval interval)
  weekStart =
    do
      let (y, w, _) = toWeekDateWithStart weekStart d_
      let (limitY, limitWN, _) = toWeekDateWithStart weekStart limitDay
      takeEvery interval
        $ takeWhile (<= (limitY, limitWN))
        $ weeksIntoTheFutureStartingFrom weekStart y w
