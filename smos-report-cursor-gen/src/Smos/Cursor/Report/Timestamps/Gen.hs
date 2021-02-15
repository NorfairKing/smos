{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Report.Timestamps.Gen where

import Cursor.Forest.Gen ()
import Cursor.Text.Gen ()
import Data.GenValidity
import Data.GenValidity.Path ()
import qualified Data.Map as M
import Smos.Cursor.Report.Entry.Gen
import Smos.Cursor.Report.Timestamps
import Smos.Data
import Smos.Data.Gen ()
import Test.QuickCheck

instance GenValid TimestampsReportCursor where
  genValid = TimestampsReportCursor <$> genValidEntryReportCursorWith makeTimestampsEntryCursor sortTimestampEntryCursors genEntryWithTimestamps
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

genNonEmptyTimestampsReportCursor :: Gen TimestampsReportCursor
genNonEmptyTimestampsReportCursor = TimestampsReportCursor <$> genNonEmptyValidEntryReportCursorWith makeTimestampsEntryCursor sortTimestampEntryCursors genEntryWithTimestamps

instance GenValid TimestampsEntryCursor where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

genEntryWithTimestamps :: Gen Entry
genEntryWithTimestamps = do
  name <- genValid
  timestamp <- genValid
  timestamps <- M.fromList . ((name, timestamp) :) <$> genValid
  let modEntry e = e {entryTimestamps = timestamps}
  modEntry <$> genValid
