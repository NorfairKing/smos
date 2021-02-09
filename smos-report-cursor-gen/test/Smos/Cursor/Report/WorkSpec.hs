{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.Report.WorkSpec where

import qualified Data.Map as M
import qualified Data.Set as S
import Path
import Smos.Cursor.Report.Work
import Smos.Cursor.Report.Work.Gen ()
import Smos.Data.Gen ()
import Smos.Report.Archive.Gen ()
import Smos.Report.Filter.Gen ()
import Smos.Report.ShouldPrint
import Smos.Report.Sorter.Gen ()
import Smos.Report.TestUtils
import Smos.Report.Work
import Smos.Report.Work.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @WorkReportCursor
  -- describe "workReportCursorNext" $ it "produces valid cursors" $ producesValidsOnValids workReportCursorNext
  -- describe "workReportCursorPrev" $ it "produces valid cursors" $ producesValidsOnValids workReportCursorPrev
  -- describe "workReportCursorFirst" $ it "produces valid cursors" $ producesValidsOnValids workReportCursorFirst
  -- describe "workReportCursorLast" $ it "produces valid cursors" $ producesValidsOnValids workReportCursorLast
  modifyMaxSuccess (`div` 10) $
    describe "produceWorkReportCursor" $
      it "produces valid reports for interesting stores" $
        forAllValid $ \ctx ->
          forAllValid $ \ha ->
            withInterestingStore $ \dc -> do
              wrc <- produceWorkReportCursor ha DontPrint dc ctx
              shouldBeValid wrc
