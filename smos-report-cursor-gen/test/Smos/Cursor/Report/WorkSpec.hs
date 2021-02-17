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
  describe "intermediateWorkReportToWorkReportCursor" $ it "produces valid cursors" $ producesValidsOnValids2 intermediateWorkReportToWorkReportCursor
  describe "workReportCursorNext" $ do
    it "produces valid cursors" $ producesValidsOnValids workReportCursorNext
  -- TODO: not sure why this doesn't seem to hold.
  --  it "is the inverse of workReportCursorPrev" $
  --    forAllValid $ \wrc ->
  --      case workReportCursorPrev wrc of
  --        Nothing -> pure ()
  --        Just wrc' -> workReportCursorNext wrc' `shouldBe` Just wrc
  describe "workReportCursorPrev" $ do
    it "produces valid cursors" $ producesValidsOnValids workReportCursorPrev
  -- TODO: not sure why this doesn't seem to hold.
  --  it "is the inverse of workReportCursorNext" $
  --    forAllValid $ \wrc ->
  --      case workReportCursorNext wrc of
  --        Nothing -> pure ()
  --        Just wrc' -> workReportCursorPrev wrc' `shouldBe` Just wrc
  describe "workReportCursorFirst" $ do
    it "produces valid cursors" $ producesValidsOnValids workReportCursorFirst
    it "is idempotent" $
      forAllValid $ \wrc ->
        workReportCursorFirst (workReportCursorFirst wrc) `shouldBe` workReportCursorFirst wrc
    it "is the same as last first" $
      forAllValid $ \wrc ->
        workReportCursorFirst (workReportCursorLast wrc) `shouldBe` workReportCursorFirst wrc
  describe "workReportCursorLast" $ do
    it "produces valid cursors" $ producesValidsOnValids workReportCursorLast
    it "is idempotent" $
      forAllValid $ \wrc ->
        workReportCursorLast (workReportCursorLast wrc) `shouldBe` workReportCursorLast wrc
    it "is the same as first last" $
      forAllValid $ \wrc ->
        workReportCursorLast (workReportCursorFirst wrc) `shouldBe` workReportCursorLast wrc
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
