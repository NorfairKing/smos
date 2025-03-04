{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Scheduler.Schedule.Gen where

import Data.GenValidity.Path ()
import Smos.Scheduler.Render.Gen ()
import Smos.Scheduler.Schedule
import System.Cron as Cron
import Test.QuickCheck
import Test.Syd.Validity

instance GenValid ScheduleItem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid DestinationPathTemplate where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ScheduleTemplate where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid EntryTemplate where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid TimestampTemplate where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Recurrence where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid CronSchedule where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance Validity Cron.MinuteSpec

instance GenValid Cron.MinuteSpec where
  genValid = genValid `suchThatMap` mkMinuteSpec

instance Validity Cron.HourSpec

instance GenValid Cron.HourSpec where
  genValid = genValid `suchThatMap` mkHourSpec

instance Validity Cron.DayOfMonthSpec

instance GenValid Cron.DayOfMonthSpec where
  genValid = genValid `suchThatMap` mkDayOfMonthSpec

instance Validity Cron.MonthSpec

instance GenValid Cron.MonthSpec where
  genValid = genValid `suchThatMap` mkMonthSpec

instance Validity Cron.DayOfWeekSpec

instance GenValid Cron.DayOfWeekSpec where
  genValid = genValid `suchThatMap` mkDayOfWeekSpec

instance Validity CronField

instance GenValid CronField

instance Validity Cron.BaseField

instance GenValid Cron.BaseField

instance Validity Cron.StepField

instance GenValid Cron.StepField

instance Validity Cron.SpecificField

instance GenValid Cron.SpecificField

instance Validity Cron.RangeField

instance GenValid Cron.RangeField
