{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Report.Work.Gen where

import Cursor.Forest.Gen ()
import Cursor.Text.Gen ()
import Data.GenValidity
import Data.GenValidity.Path ()
import Data.Maybe
import Smos.Cursor.Report.Entry
import Smos.Cursor.Report.Entry.Gen ()
import Smos.Cursor.Report.Stuck
import Smos.Cursor.Report.Stuck.Gen
import Smos.Cursor.Report.Timestamps
import Smos.Cursor.Report.Timestamps.Gen
import Smos.Cursor.Report.Waiting
import Smos.Cursor.Report.Waiting.Gen
import Smos.Cursor.Report.Work
import Smos.Data.Gen ()
import Test.QuickCheck

instance GenValid WorkReportCursor where
  -- Turn off shrinking, because it seems to loop.
  -- TODO: Figure out why and fix it.
  shrinkValid _ = []

  genValid =
    ( do
        wrc@WorkReportCursor {..} <- genValidStructurallyWithoutExtraChecking
        case workReportCursorSelection of
          NextBeginSelected ->
            if isJust workReportCursorNextBeginCursor
              then pure wrc
              else do
                nbc <- genValid
                pure $ wrc {workReportCursorNextBeginCursor = Just nbc}
          DeadlinesSelected ->
            if isJust $ entryReportCursorSelectedEntryReportEntryCursors $ timestampsReportCursorEntryReportCursor workReportCursorDeadlinesCursor
              then pure wrc
              else do
                tsrc <- genNonEmptyTimestampsReportCursor
                pure $ wrc {workReportCursorDeadlinesCursor = tsrc}
          WaitingSelected ->
            if isJust $ entryReportCursorSelectedEntryReportEntryCursors $ waitingReportCursorEntryReportCursor workReportCursorOverdueWaiting
              then pure wrc
              else do
                warc <- genNonEmptyWaitingReportCursor
                pure $ wrc {workReportCursorOverdueWaiting = warc}
          StuckSelected ->
            if isJust (stuckReportCursorNonEmptyCursor workReportCursorOverdueStuck)
              then pure wrc
              else do
                neStuckReport <- genNonEmptyStuckReportCursor
                pure $ wrc {workReportCursorOverdueStuck = neStuckReport}
          ResultsSelected -> pure wrc
    )
      `suchThat` isValid

instance GenValid WorkReportCursorSelection where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
