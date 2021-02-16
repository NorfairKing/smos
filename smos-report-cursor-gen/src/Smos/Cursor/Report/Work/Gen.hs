{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Report.Work.Gen where

import Cursor.Forest.Gen ()
import Cursor.Map.Gen ()
import Cursor.Text.Gen ()
import Data.GenValidity
import Data.GenValidity.Path ()
import Smos.Cursor.Report.Entry
import Smos.Cursor.Report.Entry.Gen
import Smos.Cursor.Report.Stuck
import Smos.Cursor.Report.Stuck.Gen
import Smos.Cursor.Report.Timestamps
import Smos.Cursor.Report.Timestamps.Gen
import Smos.Cursor.Report.Waiting
import Smos.Cursor.Report.Waiting.Gen
import Smos.Cursor.Report.Work
import Smos.Data.Gen ()
import Smos.Report.Filter.Gen ()
import Test.QuickCheck

instance GenValid WorkReportCursor where
  shrinkValid wrc =
    let nextBeginEmpty = [wrc {workReportCursorNextBeginCursor = Nothing} | not $ workReportNextBeginEmpty wrc]
        entriesWithoutContextEmpty = [wrc {workReportCursorEntriesWithoutContext = emptyEntryReportCursor} | not $ workReportWithoutContextEmpty wrc]
        checkViolationsEmpty = [wrc {workReportCursorCheckViolations = Nothing} | not $ workReportCheckViolationsEmpty wrc]
        deadlinesEmpty = [wrc {workReportCursorDeadlinesCursor = emptyTimestampsReportCursor} | not $ workReportDeadlinesEmpty wrc]
        waitingEmpty = [wrc {workReportCursorOverdueWaiting = emptyWaitingReportCursor} | not $ workReportOverdueWaitingEmpty wrc]
        stuckEmpty = [wrc {workReportCursorOverdueStuck = emptyStuckReportCursor} | not $ workReportOverdueStuckEmpty wrc]
        resultsEmpty = [wrc {workReportCursorResultEntries = emptyEntryReportCursor} | not $ workReportResultsEmpty wrc]
        resultsSelected = [wrc {workReportCursorSelection = ResultsSelected} | workReportCursorSelection wrc /= ResultsSelected]
     in concat
          [ nextBeginEmpty,
            entriesWithoutContextEmpty,
            checkViolationsEmpty,
            deadlinesEmpty,
            waitingEmpty,
            stuckEmpty,
            resultsEmpty,
            resultsSelected
          ]

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
          CheckViolationsSelected ->
            if workReportCheckViolationsEmpty wrc
              then do
                mc <- genValid
                pure $ wrc {workReportCursorCheckViolations = Just mc}
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
