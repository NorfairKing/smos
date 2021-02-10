{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Report.Work.Gen where

import Cursor.Forest.Gen ()
import Cursor.Text.Gen ()
import Data.GenValidity
import Data.GenValidity.Path ()
import Smos.Cursor.Report.Entry.Gen ()
import Smos.Cursor.Report.Work
import Smos.Data.Gen ()

instance GenValid WorkReportCursor where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid WorkReportCursorSelection where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
