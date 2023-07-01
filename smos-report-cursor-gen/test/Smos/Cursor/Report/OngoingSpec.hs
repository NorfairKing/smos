{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.Report.OngoingSpec where

import Smos.Cursor.Report.Ongoing
import Smos.Cursor.Report.Ongoing.Gen ()
import Smos.Directory.Archive.Gen ()
import Smos.Directory.ShouldPrint
import Smos.Directory.TestUtils
import Smos.Report.Filter.Gen ()
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Lens

spec :: Spec
spec = do
  genValidSpec @OngoingReportCursor
  describe "ongoingReportCursorNext" $ it "produces valid cursors" $ producesValid ongoingReportCursorNext
  describe "ongoingReportCursorPrev" $ it "produces valid cursors" $ producesValid ongoingReportCursorPrev
  describe "ongoingReportCursorFirst" $ it "produces valid cursors" $ producesValid ongoingReportCursorFirst
  describe "ongoingReportCursorLast" $ it "produces valid cursors" $ producesValid ongoingReportCursorLast
  describe "ongoingReportCursorSelectReport" $ it "produces valid cursors" $ producesValid ongoingReportCursorSelectReport
  describe "ongoingReportCursorSelectFilter" $ it "produces valid cursors" $ producesValid ongoingReportCursorSelectFilter
  describe "ongoingReportCursorInsert" $ it "produces valid cursors" $ producesValid2 ongoingReportCursorInsert
  describe "ongoingReportCursorAppend" $ it "produces valid cursors" $ producesValid2 ongoingReportCursorAppend
  describe "ongoingReportCursorRemove" $ it "produces valid cursors" $ producesValid ongoingReportCursorRemove
  describe "ongoingReportCursorDelete" $ it "produces valid cursors" $ producesValid ongoingReportCursorDelete
  describe "ongoingReportCursorEntryReportCursorL" $ lensSpec ongoingReportCursorEntryReportCursorL
  describe "makeOngoingEntryCursor" $
    it "produces valid cursors" $
      forAllValid $ \zone ->
        forAllValid $ \now -> producesValid $ makeOngoingEntryCursor zone now
  describe "makeOngoingEntryCursor'" $
    it "produces valid cursors" $
      forAllValid $ \zone ->
        forAllValid $ \now -> producesValid2 $ makeOngoingEntryCursor' zone now
  modifyMaxSuccess (`div` 10) $
    describe "produceOngoingReportCursor" $
      it "produces valid reports for interesting stores" $
        forAllValid $ \zone ->
          forAllValid $ \now ->
            forAllValid $ \mf ->
              forAllValid $ \ha ->
                withInterestingStore $ \dc -> do
                  wrc <- produceOngoingReportCursor zone now mf ha DontPrint dc
                  shouldBeValid wrc
