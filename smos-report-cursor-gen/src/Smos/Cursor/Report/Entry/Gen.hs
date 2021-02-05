{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Report.Entry.Gen where

import Cursor.Forest.Gen ()
import Cursor.Text.Gen ()
import Data.GenValidity
import Data.GenValidity.Path ()
import Smos.Cursor.Report.Entry
import Smos.Data.Gen ()

instance GenValid EntryReportCursor where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid EntryReportCursorSelection where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid EntryReportEntryCursor where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
