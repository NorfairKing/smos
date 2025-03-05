{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Scheduler.History.Gen where

import Data.GenValidity.Time ()
import Smos.Scheduler.History
import Smos.Scheduler.OptParse
import Smos.Scheduler.Recurrence
import Smos.Scheduler.Render
import System.Cron as Cron
import Test.QuickCheck
import Test.Syd.Validity

instance GenValid LatestActivation where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
