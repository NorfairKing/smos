{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Report.Ongoing.Gen where

import Data.GenValidity
import Smos.Cursor.Report.Entry.Gen ()
import Smos.Cursor.Report.Ongoing
import Smos.Report.Ongoing.Gen ()

instance GenValid OngoingReportCursor where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
