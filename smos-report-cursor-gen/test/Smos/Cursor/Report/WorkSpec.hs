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
  -- describe "workReportCursorSelectionL" $ lensSpec workReportCursorSelectionL
  describe "emptyWorkReportCursor" $ it "is valid" $ shouldBeValid emptyWorkReportCursor
  describe "workReportCursorNext" $ do
    it "produces valid cursors" $ producesValid workReportCursorNext
  -- TODO: not sure why this doesn't seem to hold.
  --  it "is the inverse of workReportCursorPrev" $
  --    forAllValid $ \wrc ->
  --      case workReportCursorPrev wrc of
  --        Nothing -> pure ()
  --        Just wrc' -> workReportCursorNext wrc' `shouldBe` Just wrc
  describe "workReportCursorPrev" $ do
    it "produces valid cursors" $ producesValid workReportCursorPrev
  -- TODO: not sure why this doesn't seem to hold.
  --  it "is the inverse of workReportCursorNext" $
  --    forAllValid $ \wrc ->
  --      case workReportCursorNext wrc of
  --        Nothing -> pure ()
  --        Just wrc' -> workReportCursorPrev wrc' `shouldBe` Just wrc
  describe "intermediateWorkReportToWorkReportCursor" $ it "produces valid cursors" $ producesValid2 intermediateWorkReportToWorkReportCursor
  describe "workReportCursorFirst" $ do
    it "produces valid cursors" $ producesValid workReportCursorFirst
    it "is idempotent" $
      forAllValid $ \wrc ->
        workReportCursorFirst (workReportCursorFirst wrc) `shouldBe` workReportCursorFirst wrc
    it "is the same as last first" $
      forAllValid $ \wrc ->
        workReportCursorFirst (workReportCursorLast wrc) `shouldBe` workReportCursorFirst wrc
  describe "workReportCursorLast" $ do
    it "produces valid cursors" $ producesValid workReportCursorLast
    it "is idempotent" $
      forAllValid $ \wrc ->
        workReportCursorLast (workReportCursorLast wrc) `shouldBe` workReportCursorLast wrc
    it "is the same as first last" $
      forAllValid $ \wrc ->
        workReportCursorLast (workReportCursorFirst wrc) `shouldBe` workReportCursorLast wrc
  describe "workReportCursorSelectReport" $ it "produces valid cursors" $ producesValid workReportCursorSelectReport
  describe "workReportCursorSelectFilter" $ it "produces valid cursors" $ producesValid workReportCursorSelectFilter
  describe "workReportCursorInsert" $ it "produces valid cursors" $ producesValid2 workReportCursorInsert
  describe "workReportCursorAppend" $ it "produces valid cursors" $ producesValid2 workReportCursorAppend
  describe "workReportCursorRemove" $ it "produces valid cursors" $ producesValid workReportCursorRemove
  describe "workReportCursorDelete" $ it "produces valid cursors" $ producesValid workReportCursorDelete
  modifyMaxSuccess (`div` 10) $
    describe "produceWorkReportCursor" $
      it "produces valid reports for interesting stores" $
        forAllValid $ \ctx ->
          forAllValid $ \ha ->
            withInterestingStore $ \dc -> do
              wrc <- produceWorkReportCursor ha DontPrint dc ctx
              shouldBeValid wrc
