{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.Report.TimestampsSpec where

import Smos.Cursor.Report.Timestamps
import Smos.Cursor.Report.Timestamps.Gen ()
import Smos.Report.Archive.Gen
import Smos.Report.ShouldPrint
import Smos.Report.TestUtils
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @TimestampsReportCursor
  genValidSpec @TimestampsEntryCursor
  describe "timestampsReportCursorNext" $ it "produces valid cursors" $ producesValidsOnValids timestampsReportCursorNext
  describe "timestampsReportCursorPrev" $ it "produces valid cursors" $ producesValidsOnValids timestampsReportCursorPrev
  describe "timestampsReportCursorFirst" $ it "produces valid cursors" $ producesValidsOnValids timestampsReportCursorFirst
  describe "timestampsReportCursorLast" $ it "produces valid cursors" $ producesValidsOnValids timestampsReportCursorLast
  describe "makeTimestampsEntryCursor" $ it "produces valid cursors" $ producesValidsOnValids2 makeTimestampsEntryCursor
  modifyMaxSuccess (`div` 10) $
    describe "produceTimestampsReportCursor" $
      it "produces valid reports for interesting stores" $
        forAllValid $ \ha ->
          withInterestingStore $ \dc -> do
            wrc <- produceTimestampsReportCursor ha DontPrint dc
            shouldBeValid wrc
