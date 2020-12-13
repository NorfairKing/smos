{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.Report.NextSpec where

import Smos.Cursor.Report.Next
import Smos.Cursor.Report.Next.Gen ()
import Smos.Report.TestUtils
import Test.Syd

import Test.Syd.Validity
import Test.Syd.Validity.Lens

spec :: Spec
spec = do
  genValidSpec @NextActionReportCursor
  genValidSpec @NextActionReportCursorSelection
  genValidSpec @NextActionEntryCursor
  describe "nextActionReportCursorNextActionEntryCursorsL" $ lensSpecOnValid nextActionReportCursorNextActionEntryCursorsL
  describe "nextActionReportCursorSelectedNextActionEntryCursorsL" $ lensSpecOnValid nextActionReportCursorSelectedNextActionEntryCursorsL
  describe "nextActionReportCursorSelectionL" $ lensSpecOnValid nextActionReportCursorSelectionL
  describe "nextActionReportCursorFilterBarL" $ lensSpecOnValid nextActionReportCursorFilterBarL
  describe "makeNextActionEntryCursor" $ it "produces valid cursors" $ producesValidsOnValids2 makeNextActionEntryCursor
  modifyMaxSuccess (`div` 10) $
    describe "produceNextActionReportCursor" $
      it "produces valid reports for interesting stores" $
        withInterestingStore $
          \dc -> do
            narc <- produceNextActionReportCursor dc
            shouldBeValid narc
