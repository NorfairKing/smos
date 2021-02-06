{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.Report.NextSpec where

import Smos.Cursor.Report.Next
import Smos.Cursor.Report.Next.Gen ()
import Smos.Report.Archive.Gen ()
import Smos.Report.Filter.Gen ()
import Smos.Report.ShouldPrint
import Smos.Report.TestUtils
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Lens

spec :: Spec
spec = do
  genValidSpec @NextActionReportCursor
  describe "nextActionReportCursorNext" $ it "produces valid cursors" $ producesValidsOnValids nextActionReportCursorNext
  describe "nextActionReportCursorPrev" $ it "produces valid cursors" $ producesValidsOnValids nextActionReportCursorPrev
  describe "nextActionReportCursorFirst" $ it "produces valid cursors" $ producesValidsOnValids nextActionReportCursorFirst
  describe "nextActionReportCursorLast" $ it "produces valid cursors" $ producesValidsOnValids nextActionReportCursorLast
  describe "nextActionReportCursorSelectReport" $ it "produces valid cursors" $ producesValidsOnValids nextActionReportCursorSelectReport
  describe "nextActionReportCursorSelectFilter" $ it "produces valid cursors" $ producesValidsOnValids nextActionReportCursorSelectFilter
  describe "nextActionReportCursorInsert" $ it "produces valid cursors" $ producesValidsOnValids2 nextActionReportCursorInsert
  describe "nextActionReportCursorAppend" $ it "produces valid cursors" $ producesValidsOnValids2 nextActionReportCursorAppend
  describe "nextActionReportCursorRemove" $ it "produces valid cursors" $ producesValidsOnValids nextActionReportCursorRemove
  describe "nextActionReportCursorDelete" $ it "produces valid cursors" $ producesValidsOnValids nextActionReportCursorDelete
  describe "nextActionReportCursorEntryReportCursorL" $ lensSpecOnValid nextActionReportCursorEntryReportCursorL
  describe "makeNextActionEntryCursor" $ it "produces valid cursors" $ producesValidsOnValids makeNextActionEntryCursor
  describe "makeNextActionEntryCursor'" $ it "produces valid cursors" $ producesValidsOnValids2 makeNextActionEntryCursor'
  modifyMaxSuccess (`div` 10) $
    describe "produceNextActionReportCursor" $
      it "produces valid reports for interesting stores" $
        forAllValid $ \mf ->
          forAllValid $ \ha ->
            withInterestingStore $ \dc -> do
              wrc <- produceNextActionReportCursor mf ha DontPrint dc
              shouldBeValid wrc
