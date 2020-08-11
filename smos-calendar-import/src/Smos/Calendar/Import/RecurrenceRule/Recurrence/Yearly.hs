-- This module uses list as a monad a lot, make sure you understand it before reading this module.
module Smos.Calendar.Import.RecurrenceRule.Recurrence.Yearly where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time
import Smos.Calendar.Import.RecurrenceRule.Recurrence.Util
import Smos.Calendar.Import.RecurrenceRule.Type
import Smos.Calendar.Import.WeekDate

yearlyDateTimeRecurrence ::
  LocalTime ->
  LocalTime ->
  Interval ->
  Set ByMonth ->
  DayOfWeek ->
  Set ByWeekNo ->
  Set ByYearDay ->
  Set ByMonthDay ->
  Set ByDay ->
  Set ByHour ->
  Set ByMinute ->
  Set BySecond ->
  Set BySetPos ->
  [LocalTime]
yearlyDateTimeRecurrence
  lt@(LocalTime d_ tod_)
  limit@(LocalTime limitDay _)
  interval
  byMonths
  weekStart
  byWeekNos
  byYearDays
  byMonthDays
  byDays
  byHours
  byMinutes
  bySeconds
  bySetPoss = do
    let (y_, m_, md_) = toGregorian d_
    year <- yearlyYearRecurrence d_ limitDay interval
    -- let (_, weekNo, _) = toWeekDateWithStart weekStart d_
    -- weekNo <- byWeekNoExpand weekStart year weekNo byWeekNos
    d <- maybeToList $ fromGregorianValid year m_ md_
    tod <- timeOfDayExpand tod_ byHours byMinutes bySeconds
    let next = LocalTime d tod
    guard (next > lt) -- Don't take the current one again
    guard (next <= limit) -- Don't go beyond the limit
    pure next

yearlyDateRecurrence ::
  Day ->
  Day ->
  Interval ->
  Set ByMonth ->
  DayOfWeek ->
  Set ByWeekNo ->
  Set ByYearDay ->
  Set ByMonthDay ->
  Set ByDay ->
  Set BySetPos ->
  [Day]
yearlyDateRecurrence
  d_
  limitDay
  interval
  byMonths
  weekStart
  byWeekNos
  byYearDays
  byMonthDays
  byDays
  bySetPoss = do
    let (y_, m_, md_) = toGregorian d_
    year <- yearlyYearRecurrence d_ limitDay interval
    -- let (_, weekNo, _) = toWeekDateWithStart weekStart d_
    -- weekNo <- byWeekNoExpand weekStart year weekNo byWeekNos
    d <- maybeToList $ fromGregorianValid year m_ md_
    guard (d <= limitDay)
    guard (d > d_) -- Don't take the current one again
    pure d

yearlyYearRecurrence ::
  Day ->
  Day ->
  Interval ->
  [Integer]
yearlyYearRecurrence d_ limitDay (Interval interval) = do
  let (year_, _, _) = toGregorian d_
  let (limitYear, _, _) = toGregorian limitDay
  takeEvery interval
    $ takeWhile (<= limitYear)
    $ dropWhile
      (< year_)
      [year_ ..]
