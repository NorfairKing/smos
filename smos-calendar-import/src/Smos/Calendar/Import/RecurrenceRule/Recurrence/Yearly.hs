-- This module uses list as a monad a lot, make sure you understand it before reading this module.
module Smos.Calendar.Import.RecurrenceRule.Recurrence.Yearly where

import Control.Monad
import Data.List
import qualified Data.List.NonEmpty as NE
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
    month_ <- maybeToList $ monthNoToMonth m_
    d <- yearlyDayCandidate d_ weekStart year byMonths byWeekNos
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
    month_ <- maybeToList $ monthNoToMonth m_
    let mMonth = byMonthExpand byMonths
    let mWeekNos = byWeekNoExpand weekStart year byWeekNos
    d <- yearlyDayCandidate d_ weekStart year byMonths byWeekNos
    guard (d <= limitDay)
    guard (d > d_) -- Don't take the current one again
    pure d

yearlyDayCandidate d_ weekStart year byMonths byWeekNos = do
  let (y_, m_, md_) = toGregorian d_
  month_ <- maybeToList $ monthNoToMonth m_
  let mMonth = byMonthExpand byMonths
  let mWeekNos = byWeekNoExpand weekStart year byWeekNos
  case mWeekNos of
    Nothing -> case mMonth of
      Nothing -> maybeToList $ fromGregorianValid year (monthToMonthNo month_) md_
      Just months -> do
        month <- NE.toList months
        maybeToList $ fromGregorianValid year (monthToMonthNo month) md_
    Just wnos -> do
      wno <- NE.toList wnos
      dow <- [Monday .. Sunday]
      d' <- maybeToList $ fromWeekDateWithStart weekStart year wno dow
      let (y', m', md') = toGregorian d'
      condition <- case mMonth of
        Nothing -> pure $ y' == year
        Just months -> do
          month <- NE.toList months
          pure $ y' == year && m' == monthToMonthNo month
      if condition
        then pure d'
        else []

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
