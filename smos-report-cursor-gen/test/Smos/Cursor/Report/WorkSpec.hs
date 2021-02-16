{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.Report.WorkSpec where

import Smos.Cursor.Report.Work
import Smos.Cursor.Report.Work.Gen ()
import Smos.Data.Gen ()
import Smos.Report.Archive.Gen ()
import Smos.Report.Filter.Gen ()
import Smos.Report.ShouldPrint
import Smos.Report.Sorter.Gen ()
import Smos.Report.TestUtils
import Smos.Report.Work.Gen ()
import Test.Syd
import Test.Syd.Validity

-- import Test.Syd.Validity.Lens

spec :: Spec
spec = do
  genValidSpec @WorkReportCursor
  -- Does not hold because of the validity constraint
  -- describe "workReportCursorSelectionL" $ lensSpecOnValid workReportCursorSelectionL
  describe "emptyWorkReportCursor" $ it "is valid" $ shouldBeValid emptyWorkReportCursor
  describe "intermediateWorkReportToWorkReportCursor" $ it "produces valid cursors" $ producesValidsOnValids intermediateWorkReportToWorkReportCursor
  describe "workReportCursorNext" $ do
    it "produces valid cursors" $ producesValidsOnValids workReportCursorNext
    it "is the inverse of workReportCursorPrev" $ inverseFunctionsIfSucceedOnValid workReportCursorPrev workReportCursorNext
  describe "workReportCursorPrev" $ do
    it "produces valid cursors" $ producesValidsOnValids workReportCursorPrev
    it "is the inverse of workReportCursorNext" $ inverseFunctionsIfSucceedOnValid workReportCursorNext workReportCursorPrev
  describe "workReportCursorFirst" $ it "produces valid cursors" $ producesValidsOnValids workReportCursorFirst
  describe "workReportCursorLast" $ it "produces valid cursors" $ producesValidsOnValids workReportCursorLast
  describe "workReportCursorSelectReport" $ it "produces valid cursors" $ producesValidsOnValids workReportCursorSelectReport
  describe "workReportCursorSelectFilter" $ it "produces valid cursors" $ producesValidsOnValids workReportCursorSelectFilter
  describe "workReportCursorInsert" $ it "produces valid cursors" $ producesValidsOnValids2 workReportCursorInsert
  describe "workReportCursorAppend" $ it "produces valid cursors" $ producesValidsOnValids2 workReportCursorAppend
  describe "workReportCursorRemove" $ it "produces valid cursors" $ producesValidsOnValids workReportCursorRemove
  describe "workReportCursorDelete" $ it "produces valid cursors" $ producesValidsOnValids workReportCursorDelete
  modifyMaxSuccess (`div` 10) $
    describe "produceWorkReportCursor" $
      it "produces valid reports for interesting stores" $
        forAllValid $ \ctx ->
          forAllValid $ \ha ->
            withInterestingStore $ \dc -> do
              wrc <- produceWorkReportCursor ha DontPrint dc ctx
              shouldBeValid wrc
