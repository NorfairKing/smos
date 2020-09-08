{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Report.Waiting.Gen where

import Cursor.Forest.Gen ()
import Cursor.Text.Gen ()
import Data.GenValidity
import Data.GenValidity.Path ()
import Smos.Cursor.Report.Waiting
import Smos.Data.Gen ()
import Smos.Report.Path.Gen ()

instance GenValid WaitingReportCursor where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid WaitingEntryCursor where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
