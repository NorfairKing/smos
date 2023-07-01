{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.Ongoing.Gen where

import Data.GenValidity
import Data.GenValidity.Path ()
import Smos.Data.Gen ()
import Smos.Report.Ongoing
import Smos.Report.Period.Gen ()
import Smos.Report.Time.Gen ()
import Smos.Report.TimeBlock.Gen ()

instance GenValid OngoingReport where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid OngoingEntry where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid BeginEnd where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
