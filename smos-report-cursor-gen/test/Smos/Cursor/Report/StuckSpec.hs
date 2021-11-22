{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.Report.StuckSpec where

import Smos.Cursor.Report.Stuck
import Smos.Cursor.Report.Stuck.Gen ()
import Smos.Report.Archive.Gen ()
import Smos.Report.Filter.Gen ()
import Smos.Report.ShouldPrint
import Smos.Report.TestUtils
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @StuckReportCursor
  describe "stuckReportCursorNext" $ it "produces valid cursors" $ producesValid stuckReportCursorNext
  describe "stuckReportCursorPrev" $ it "produces valid cursors" $ producesValid stuckReportCursorPrev
  describe "stuckReportCursorFirst" $ it "produces valid cursors" $ producesValid stuckReportCursorFirst
  describe "stuckReportCursorLast" $ it "produces valid cursors" $ producesValid stuckReportCursorLast
  -- Does not hold because of the validity constraint
  -- describe "stuckReportCursorNonEmptyCursorL" $ lensSpec stuckReportCursorNonEmptyCursorL
  modifyMaxSuccess (`div` 10) $
    describe "produceStuckReportCursor" $
      it "produces valid reports for interesting stores" $
        forAllValid $ \tz ->
          withInterestingStore $ \dc -> do
            wrc <- produceStuckReportCursor tz DontPrint dc
            shouldBeValid wrc
