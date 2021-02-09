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
import Test.Syd.Validity.Lens

spec :: Spec
spec = do
  genValidSpec @StuckReportCursor
  describe "stuckReportCursorNext" $ it "produces valid cursors" $ producesValidsOnValids stuckReportCursorNext
  describe "stuckReportCursorPrev" $ it "produces valid cursors" $ producesValidsOnValids stuckReportCursorPrev
  describe "stuckReportCursorFirst" $ it "produces valid cursors" $ producesValidsOnValids stuckReportCursorFirst
  describe "stuckReportCursorLast" $ it "produces valid cursors" $ producesValidsOnValids stuckReportCursorLast
  -- Does not hold because of the validity constraint
  -- describe "stuckReportCursorNonEmptyCursorL" $ lensSpecOnValid stuckReportCursorNonEmptyCursorL
  modifyMaxSuccess (`div` 10) $
    describe "produceStuckReportCursor" $
      it "produces valid reports for interesting stores" $
        forAllValid $ \tz ->
          forAllValid $ \ha ->
            withInterestingStore $ \dc -> do
              wrc <- produceStuckReportCursor tz ha DontPrint dc
              shouldBeValid wrc
