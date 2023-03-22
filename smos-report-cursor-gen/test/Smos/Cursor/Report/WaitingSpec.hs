{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.Report.WaitingSpec where

import Smos.Cursor.Report.Waiting
import Smos.Cursor.Report.Waiting.Gen ()
import Smos.Directory.Archive.Gen ()
import Smos.Directory.ShouldPrint
import Smos.Directory.TestUtils
import Smos.Report.Filter.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @WaitingReportCursor
  describe "waitingReportCursorNext" $ it "produces valid cursors" $ producesValid waitingReportCursorNext
  describe "waitingReportCursorPrev" $ it "produces valid cursors" $ producesValid waitingReportCursorPrev
  describe "waitingReportCursorFirst" $ it "produces valid cursors" $ producesValid waitingReportCursorFirst
  describe "waitingReportCursorLast" $ it "produces valid cursors" $ producesValid waitingReportCursorLast
  describe "waitingReportCursorSelectReport" $ it "produces valid cursors" $ producesValid waitingReportCursorSelectReport
  describe "waitingReportCursorSelectFilter" $ it "produces valid cursors" $ producesValid waitingReportCursorSelectFilter
  describe "waitingReportCursorInsert" $ it "produces valid cursors" $ producesValid2 waitingReportCursorInsert
  describe "waitingReportCursorAppend" $ it "produces valid cursors" $ producesValid2 waitingReportCursorAppend
  describe "waitingReportCursorRemove" $ it "produces valid cursors" $ producesValid waitingReportCursorRemove
  describe "waitingReportCursorDelete" $ it "produces valid cursors" $ producesValid waitingReportCursorDelete
  -- Does not hold because of the validity constraint
  -- describe "waitingReportCursorEntryReportCursorL" $ lensSpec waitingReportCursorEntryReportCursorL
  describe "makeWaitingEntryCursor" $ it "produces valid cursors" $ producesValid2 makeWaitingEntryCursor
  describe "makeWaitingEntryCursor'" $ it "produces valid cursors" $ producesValid2 makeWaitingEntryCursor'
  modifyMaxSuccess (`div` 10) $
    describe "produceWaitingReportCursor" $
      it "produces valid reports for interesting stores" $
        forAllValid $ \mf ->
          forAllValid $ \ha ->
            withInterestingStore $ \dc -> do
              wrc <- produceWaitingReportCursor mf ha DontPrint dc
              shouldBeValid wrc
