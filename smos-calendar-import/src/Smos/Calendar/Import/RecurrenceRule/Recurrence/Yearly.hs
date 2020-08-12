-- This module uses list as a monad a lot, make sure you understand it before reading this module.
module Smos.Calendar.Import.RecurrenceRule.Recurrence.Yearly where

import Control.Monad
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Set (Set)
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Debug.Trace
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
    year <- yearlyYearRecurrence d_ limitDay interval
    d <- yearlyDayCandidate d_ weekStart year byMonths byWeekNos byYearDays
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
    year <- yearlyYearRecurrence d_ limitDay interval
    d <- yearlyDayCandidate d_ weekStart year byMonths byWeekNos byYearDays
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

yearlyDayCandidate :: Day -> DayOfWeek -> Integer -> Set ByMonth -> Set ByWeekNo -> Set ByYearDay -> [Day]
yearlyDayCandidate d_ weekStart year byMonths byWeekNos byYearDays = do
  let (_, m_, md_) = toGregorian d_
  month_ <- maybeToList $ monthNoToMonth m_
  let mMonth = byMonthExpand byMonths
  let mWeekNos = byWeekNoExpand weekStart year byWeekNos
  let mYearDays = byYearDayExpand year byYearDays
  cand <- case mWeekNos of
    Nothing -> case mMonth of
      Nothing -> case mYearDays of
        Nothing -> maybeToList $ fromGregorianValid year (monthToMonthNo month_) md_
        Just yds -> do
          yd <- NE.toList yds
          maybeToList $ fromOrdinalDateValid year $ fromIntegral yd
      Just ms -> do
        month <- NE.toList ms
        d' <- maybeToList $ fromGregorianValid year (monthToMonthNo month) md_
        condition <- case mYearDays of
          Nothing -> pure True
          Just yds -> do
            yd <- NE.toList yds
            let (_, yd') = toOrdinalDate d'
            pure $ fromIntegral yd == yd'
        guard condition
        pure d'
    Just wnos -> do
      wno <- NE.toList wnos
      dow <- [Monday .. Sunday]
      d' <- maybeToList $ fromWeekDateWithStart weekStart year wno dow
      let (y', m', _) = toGregorian d'
      monthCondition <- case mMonth of
        Nothing -> pure True
        Just ms -> do
          month <- NE.toList ms
          pure $ m' == monthToMonthNo month
      guard monthCondition
      yearDayCondition <- case mYearDays of
        Nothing -> pure True
        Just yds -> do
          yd <- NE.toList yds
          let (_, yd') = toOrdinalDate d'
          pure $ fromIntegral yd == yd'
      guard yearDayCondition
      pure d'
  let (candY, _, _) = toGregorian cand
  guard $ year == candY
  pure cand
