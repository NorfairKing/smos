{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.Report.NextSpec where

import Smos.Cursor.Report.Next
import Smos.Cursor.Report.Next.Gen ()
import Smos.Directory.TestUtils
import Smos.Report.Archive.Gen ()
import Smos.Report.Filter.Gen ()
import Smos.Report.ShouldPrint
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Lens

spec :: Spec
spec = do
  genValidSpec @NextActionReportCursor
  describe "nextActionReportCursorNext" $ it "produces valid cursors" $ producesValid nextActionReportCursorNext
  describe "nextActionReportCursorPrev" $ it "produces valid cursors" $ producesValid nextActionReportCursorPrev
  describe "nextActionReportCursorFirst" $ it "produces valid cursors" $ producesValid nextActionReportCursorFirst
  describe "nextActionReportCursorLast" $ it "produces valid cursors" $ producesValid nextActionReportCursorLast
  describe "nextActionReportCursorSelectReport" $ it "produces valid cursors" $ producesValid nextActionReportCursorSelectReport
  describe "nextActionReportCursorSelectFilter" $ it "produces valid cursors" $ producesValid nextActionReportCursorSelectFilter
  describe "nextActionReportCursorInsert" $ it "produces valid cursors" $ producesValid2 nextActionReportCursorInsert
  describe "nextActionReportCursorAppend" $ it "produces valid cursors" $ producesValid2 nextActionReportCursorAppend
  describe "nextActionReportCursorRemove" $ it "produces valid cursors" $ producesValid nextActionReportCursorRemove
  describe "nextActionReportCursorDelete" $ it "produces valid cursors" $ producesValid nextActionReportCursorDelete
  describe "nextActionReportCursorEntryReportCursorL" $ lensSpec nextActionReportCursorEntryReportCursorL
  describe "makeNextActionEntryCursor" $ it "produces valid cursors" $ producesValid makeNextActionEntryCursor
  describe "makeNextActionEntryCursor'" $ it "produces valid cursors" $ producesValid2 makeNextActionEntryCursor'
  modifyMaxSuccess (`div` 10) $
    describe "produceNextActionReportCursor" $
      it "produces valid reports for interesting stores" $
        forAllValid $ \mf ->
          forAllValid $ \ha ->
            withInterestingStore $ \dc -> do
              wrc <- produceNextActionReportCursor mf ha DontPrint dc
              shouldBeValid wrc
