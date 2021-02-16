{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Report.Work.Gen where

import Cursor.Forest.Gen ()
import Cursor.Map.Gen ()
import Cursor.Text.Gen ()
import Data.GenValidity
import Data.GenValidity.Path ()
import Smos.Cursor.Report.Entry.Gen
import Smos.Cursor.Report.Stuck.Gen
import Smos.Cursor.Report.Timestamps.Gen
import Smos.Cursor.Report.Waiting.Gen
import Smos.Cursor.Report.Work
import Smos.Data.Gen ()
import Smos.Report.Filter.Gen ()
import Test.QuickCheck

instance GenValid WorkReportCursor where
  -- Turn off shrinking, because it seems to loop.
  -- TODO: Figure out why and fix it.
  shrinkValid _ = []

  -- This mostly produces empty lists, not good either:
  -- shrinkValid wrc = do
  --   workReportCursorNextBeginCursor <- shrinkValid $ workReportCursorNextBeginCursor wrc
  --   workReportCursorDeadlinesCursor <- shrinkValid $ workReportCursorDeadlinesCursor wrc
  --   workReportCursorOverdueWaiting <- shrinkValid $ workReportCursorOverdueWaiting wrc
  --   workReportCursorOverdueStuck <- shrinkValid $ workReportCursorOverdueStuck wrc
  --   workReportCursorResultEntries <- shrinkValid $ workReportCursorResultEntries wrc
  --   workReportCursorSelection <- shrinkValid $ workReportCursorSelection wrc
  --   pure WorkReportCursor {..}

  genValid =
    ( do
        wrc@WorkReportCursor {..} <- genValidStructurallyWithoutExtraChecking
        case workReportCursorSelection of
          NextBeginSelected ->
            if workReportNextBeginEmpty wrc
              then do
                nbc <- genValid
                pure $ wrc {workReportCursorNextBeginCursor = Just nbc}
              else pure wrc
          WithoutContextSelected ->
            if workReportWithoutContextEmpty wrc
              then do
                erc <- genNonEmptyValidEntryReportCursorWith (\_ _ -> [()]) id genValid
                pure $ wrc {workReportCursorEntriesWithoutContext = erc}
              else pure wrc
          DeadlinesSelected ->
            if workReportDeadlinesEmpty wrc
              then do
                tsrc <- genNonEmptyTimestampsReportCursor
                pure $ wrc {workReportCursorDeadlinesCursor = tsrc}
              else pure wrc
          WaitingSelected ->
            if workReportOverdueWaitingEmpty wrc
              then do
                warc <- genNonEmptyWaitingReportCursor
                pure $ wrc {workReportCursorOverdueWaiting = warc}
              else pure wrc
          StuckSelected ->
            if workReportOverdueStuckEmpty wrc
              then do
                neStuckReport <- genNonEmptyStuckReportCursor
                pure $ wrc {workReportCursorOverdueStuck = neStuckReport}
              else pure wrc
          ResultsSelected -> pure wrc
    )
      `suchThat` isValid

instance GenValid WorkReportCursorSelection where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
