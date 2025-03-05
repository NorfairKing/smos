{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Scheduler.Render.Gen where

import Data.GenValidity.ByteString ()
import Data.GenValidity.Containers ()
import Data.GenValidity.Path ()
import Data.GenValidity.Time ()
import Smos.Data.Gen ()
import Smos.Report.Time.Gen ()
import Smos.Scheduler.OptParse
import Smos.Scheduler.Recurrence
import Smos.Scheduler.Render
import Smos.Scheduler.Schedule
import System.Cron as Cron
import Test.QuickCheck
import Test.Syd.Validity

instance GenValid RenderContext where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
