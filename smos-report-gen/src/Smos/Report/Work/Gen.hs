{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.Work.Gen where

import Cursor.Forest.Gen ()
import Data.GenValidity
import Data.GenValidity.Path ()
import Smos.Data.Gen ()
import Smos.Report.Agenda.Gen ()
import Smos.Report.Config.Gen ()
import Smos.Report.Filter.Gen ()
import Smos.Report.Path.Gen ()
import Smos.Report.Period.Gen ()
import Smos.Report.Sorter.Gen ()
import Smos.Report.TimeBlock.Gen ()
import Smos.Report.Work

instance GenValid WorkReportContext where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid IntermediateWorkReport where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid WorkReport where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
