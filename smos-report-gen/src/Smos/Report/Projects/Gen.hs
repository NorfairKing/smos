{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.Projects.Gen where

import Data.GenValidity
import Data.GenValidity.Path ()
import Smos.Data.Gen ()
import Smos.Report.Period.Gen ()
import Smos.Report.Projects
import Smos.Report.Time.Gen ()
import Smos.Report.TimeBlock.Gen ()

instance GenValid ProjectsReport where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ProjectEntry where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
