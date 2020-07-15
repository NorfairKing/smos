{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.Entry.Gen where

import Data.GenValidity
import Data.GenValidity.Path ()
import Smos.Data.Gen ()
import Smos.Report.Entry
import Smos.Report.Path.Gen ()
import Smos.Report.Period.Gen ()
import Smos.Report.Projection.Gen ()
import Smos.Report.TimeBlock.Gen ()

instance GenValid EntryReport where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
