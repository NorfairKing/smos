{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Scheduler.Render.Gen where

import Data.GenValidity.Containers ()
import Data.GenValidity.Path ()
import Data.GenValidity.Time ()
import Smos.Data.Gen ()
import Smos.Report.Time.Gen ()
import Smos.Scheduler.OptParse.Types
import Smos.Scheduler.Recurrence
import Smos.Scheduler.Render
import System.Cron as Cron
import Test.QuickCheck
import Test.Syd.Validity

instance GenValid RenderContext where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid ScheduleTemplate where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid EntryTemplate where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid TimestampTemplate where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid UTCTimeTemplate where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ScheduleItem where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ScheduleItemHash where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid DestinationPathTemplate where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Recurrence where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid LatestActivation where
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
