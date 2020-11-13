{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Report.Next.Gen where

import Cursor.Forest.Gen ()
import Cursor.Text.Gen ()
import Data.GenValidity
import Data.GenValidity.Path ()
import Smos.Cursor.Report.Next
import Smos.Data.Gen ()

instance GenValid NextActionReportCursor where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid NextActionReportCursorSelection where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid NextActionEntryCursor where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
