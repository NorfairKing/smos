-- This module uses list as a monad a lot, make sure you understand it before reading this module.
module Smos.Calendar.Import.RecurrenceRule.Recurrence.Monthly where

import Control.Monad
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time
import Safe
import Smos.Calendar.Import.RecurrenceRule.Recurrence.Util
import Smos.Calendar.Import.RecurrenceRule.Type

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
