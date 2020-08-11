-- This module uses list as a monad a lot, make sure you understand it before reading this module.
module Smos.Calendar.Import.RecurrenceRule.Recurrence.Monthly where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time
import Debug.Trace
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
      (year, month) <- monthlyMonthRecurrence d_ limitDay interval
      m <- maybeToList $ monthNoToMonth month
      guard $ byMonthLimitMonth byMonths m
      let (_, _, md_) = toGregorian d_
      next <- filterSetPos bySetPoss $ sort $ do
        md <- byMonthDayExpand year m md_ byMonthDays
        d' <- maybeToList $ fromGregorianValid year month md
        d <-
          if S.null byMonthDays
            then byDayExpand d' byDays
            else do
              guard $ byDayLimit byDays d'
              pure d'
        tod <- timeOfDayExpand tod_ byHours byMinutes bySeconds
        let next = LocalTime d tod
        pure next
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
      (year, month) <- monthlyMonthRecurrence d_ limitDay interval
      m <- maybeToList $ monthNoToMonth month
      guard $ byMonthLimitMonth byMonths m
      let (_, _, md_) = toGregorian d_
      d <- filterSetPos bySetPoss $ sort $ do
        md <- byMonthDayExpand year m md_ byMonthDays
        d <- maybeToList $ fromGregorianValid year month md
        if S.null byMonthDays
          then byDayExpand d byDays
          else do
            guard $ byDayLimit byDays d
            pure d
      guard (d <= limitDay)
      guard (d > d_) -- Don't take the current one again
      pure d

monthlyMonthRecurrence ::
  Day ->
  Day ->
  Interval ->
  [(Integer, Int)]
monthlyMonthRecurrence d_ limitDay (Interval interval) = do
  let (year_, month_, _) = toGregorian d_
  let (limitYear, limitMonth, _) = toGregorian limitDay
  takeEvery interval
    $ takeWhile (<= (limitYear, limitMonth))
    $ dropWhile (< (year_, month_))
    $ iterate nextMonth (year_, month_)
  where
    nextMonth :: (Integer, Int) -> (Integer, Int)
    nextMonth (y, 12) = (y + 1, 1)
    nextMonth (y, m) = (y, m + 1)
