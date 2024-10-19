{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.OptParse.Gen where

import Data.GenValidity
import Data.GenValidity.Map ()
import Smos.Directory.Gen ()
import Smos.Report.Filter.Gen ()
import Smos.Report.OptParse
import Smos.Report.Projection.Gen ()
import Smos.Report.Sorter.Gen ()

instance GenValid Configuration where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid WaitingReportConfiguration where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid StuckReportConfiguration where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid WorkReportConfiguration where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid FreeReportConfiguration where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ContextName where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
