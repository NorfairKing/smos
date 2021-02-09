{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Report.Stuck.Gen where

import Cursor.Forest.Gen ()
import Cursor.Simple.List.NonEmpty
import Cursor.Text.Gen ()
import Data.GenValidity
import Data.GenValidity.Path ()
import qualified Data.List.NonEmpty as NE
import Smos.Cursor.Report.Entry.Gen ()
import Smos.Cursor.Report.Stuck
import Smos.Data.Gen ()
import Smos.Report.Stuck
import Smos.Report.Stuck.Gen ()

instance GenValid StuckReportCursor where
  genValid = StuckReportCursor . fmap makeNonEmptyCursor . NE.nonEmpty . sortStuckEntries <$> genValid
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
