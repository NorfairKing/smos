{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Report.Work.Gen where

import Cursor.Forest.Gen ()
import Cursor.Map.Gen ()
import Cursor.Text.Gen ()
import Data.GenValidity
import Data.GenValidity.Path ()
import Data.Maybe
import Smos.Cursor.Report.Entry
import Smos.Cursor.Report.Entry.Gen
import Smos.Cursor.Report.Ongoing
import Smos.Cursor.Report.Ongoing.Gen
import Smos.Cursor.Report.Stuck
import Smos.Cursor.Report.Stuck.Gen
import Smos.Cursor.Report.Timestamps
import Smos.Cursor.Report.Timestamps.Gen
import Smos.Cursor.Report.Waiting
import Smos.Cursor.Report.Waiting.Gen
import Smos.Cursor.Report.Work
import Smos.Data.Gen ()
import Smos.Report.Filter.Gen ()

instance GenValid WorkReportCursor where
  shrinkValid wrc =
    let nextBeginEmpty = [wrc {workReportCursorNextBeginCursor = Nothing} | not $ workReportNextBeginEmpty wrc, workReportCursorSelection wrc /= NextBeginSelected]
        checkViolationsEmpty = [wrc {workReportCursorCheckViolations = Nothing} | not $ workReportCheckViolationsEmpty wrc, workReportCursorSelection wrc /= CheckViolationsSelected]
        entriesWithoutContextEmpty = [wrc {workReportCursorEntriesWithoutContext = emptyEntryReportCursor} | not $ workReportWithoutContextEmpty wrc, workReportCursorSelection wrc /= WithoutContextSelected]
        deadlinesEmpty = [wrc {workReportCursorDeadlinesCursor = emptyTimestampsReportCursor} | not $ workReportDeadlinesEmpty wrc, workReportCursorSelection wrc /= DeadlinesSelected]
        ongoingEmpty = [wrc {workReportCursorOngoingEntries = emptyOngoingReportCursor} | not $ workReportOngoingEmpty wrc, workReportCursorSelection wrc /= OngoingSelected]
        waitingEmpty = [wrc {workReportCursorOverdueWaiting = emptyWaitingReportCursor} | not $ workReportOverdueWaitingEmpty wrc, workReportCursorSelection wrc /= WaitingSelected]
        stuckEmpty = [wrc {workReportCursorOverdueStuck = emptyStuckReportCursor} | not $ workReportOverdueStuckEmpty wrc, workReportCursorSelection wrc /= StuckSelected]
        limboEmpty = [wrc {workReportCursorLimboProjects = Nothing} | isJust $ workReportCursorLimboProjects wrc, workReportCursorSelection wrc /= LimboSelected]
        resultsEmpty = [wrc {workReportCursorResultEntries = emptyEntryReportCursor} | not $ workReportResultsEmpty wrc, workReportCursorSelection wrc /= ResultsSelected]
        resultsSelected = [wrc {workReportCursorSelection = ResultsSelected} | workReportCursorSelection wrc /= ResultsSelected]
     in concat
          [ nextBeginEmpty,
            entriesWithoutContextEmpty,
            checkViolationsEmpty,
            deadlinesEmpty,
            ongoingEmpty,
            waitingEmpty,
            stuckEmpty,
            limboEmpty,
            resultsEmpty,
            resultsSelected
          ]

  genValid = do
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
      OngoingSelected ->
        if workReportOngoingEmpty wrc
          then do
            erc <- genNonEmptyOngoingReportCursor
            pure $ wrc {workReportCursorOngoingEntries = erc}
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
      LimboSelected ->
        if isNothing workReportCursorLimboProjects
          then do
            nec <- genValid
            pure $ wrc {workReportCursorLimboProjects = Just nec}
          else pure wrc
      ResultsSelected -> pure wrc

instance GenValid WorkReportCursorSelection where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
