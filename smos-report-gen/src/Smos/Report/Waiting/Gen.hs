{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.Waiting.Gen where

import Data.GenValidity
import Data.GenValidity.Path ()
import Data.List
import Smos.Data.Gen ()
import Smos.Report.Path.Gen ()
import Smos.Report.Period.Gen ()
import Smos.Report.TimeBlock.Gen ()
import Smos.Report.Waiting

instance GenValid WaitingReport where
  genValid = WaitingReport . sortOn waitingEntryTimestamp <$> genValid
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid WaitingEntry where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
