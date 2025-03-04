{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Scheduler.History.Gen where

import Data.GenValidity.Time ()
import Smos.Scheduler.History
import Test.Syd.Validity

instance GenValid LatestActivation where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
