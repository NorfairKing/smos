-- This module uses list as a monad a lot, make sure you understand it before reading this module.
module Smos.Calendar.Import.RecurrenceRule.Recurrence.Weekly where

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
