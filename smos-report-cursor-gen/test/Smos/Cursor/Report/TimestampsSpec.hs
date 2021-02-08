{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.Report.TimestampsSpec where

import Smos.Cursor.Report.Timestamps
import Smos.Cursor.Report.Timestamps.Gen ()
import Smos.Report.Archive.Gen ()
import Smos.Report.Filter.Gen ()
import Smos.Report.Period.Gen ()
import Smos.Report.ShouldPrint
import Smos.Report.TestUtils
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Lens

spec :: Spec
spec = do
  genValidSpec @TimestampsReportCursor
  genValidSpec @TimestampsEntryCursor
  describe "timestampsReportCursorNext" $ it "produces valid cursors" $ producesValidsOnValids timestampsReportCursorNext
  describe "timestampsReportCursorPrev" $ it "produces valid cursors" $ producesValidsOnValids timestampsReportCursorPrev
  describe "timestampsReportCursorFirst" $ it "produces valid cursors" $ producesValidsOnValids timestampsReportCursorFirst
  describe "timestampsReportCursorLast" $ it "produces valid cursors" $ producesValidsOnValids timestampsReportCursorLast
  describe "timestampsReportCursorSelectReport" $ it "produces valid cursors" $ producesValidsOnValids timestampsReportCursorSelectReport
  describe "timestampsReportCursorSelectFilter" $ it "produces valid cursors" $ producesValidsOnValids timestampsReportCursorSelectFilter
  describe "timestampsReportCursorInsert" $ it "produces valid cursors" $ producesValidsOnValids2 timestampsReportCursorInsert
  describe "timestampsReportCursorAppend" $ it "produces valid cursors" $ producesValidsOnValids2 timestampsReportCursorAppend
  describe "timestampsReportCursorRemove" $ it "produces valid cursors" $ producesValidsOnValids timestampsReportCursorRemove
  describe "timestampsReportCursorDelete" $ it "produces valid cursors" $ producesValidsOnValids timestampsReportCursorDelete
  -- Does not hold because of the validity constraint:
  -- describe "timestampsReportCursorEntryReportCursorL" $ lensSpecOnValid timestampsReportCursorEntryReportCursorL

  describe "makeTimestampsEntryCursor" $ it "produces valid cursors" $ producesValidsOnValids2 makeTimestampsEntryCursor
  modifyMaxSuccess (`div` 10) $
    describe "produceTimestampsReportCursor" $
      it "produces valid reports for interesting stores" $
        forAllValid $ \ha ->
          forAllValid $ \now ->
            forAllValid $ \period ->
              forAllValid $ \mf ->
                withInterestingStore $ \dc -> do
                  wrc <- produceTimestampsReportCursor now period mf ha DontPrint dc
                  shouldBeValid wrc
