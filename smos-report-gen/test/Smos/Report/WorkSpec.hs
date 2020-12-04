{-# LANGUAGE TypeApplications #-}

module Smos.Report.WorkSpec where

import Data.GenValidity.Path ()
import Smos.Data.Gen ()
import Smos.Report.Archive.Gen ()
import Smos.Report.Filter.Gen ()
import Smos.Report.ShouldPrint
import Smos.Report.TestUtils
import Smos.Report.Work
import Smos.Report.Work.Gen ()
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Validity

spec :: Spec
spec = do
  genValidSpec @WorkReportContext
  genValidSpec @IntermediateWorkReport
  genValidSpec @WorkReport
  describe "makeIntermediateWorkReport" $
    it "produces valid intermediate work reports" $
      producesValidsOnValids3 makeIntermediateWorkReport
  describe "finishWorkReport" $
    it "produces valid work reports" $
      forAllValid $ \zt ->
        forAllValid $ \pn ->
          forAllValid $ \mt ->
            forAllValid $ \ms ->
              forAllValid $ \workReport ->
                shouldBeValid $ finishWorkReport zt pn mt ms workReport
  modifyMaxSuccess (`div` 10) $
    describe "produceWorkReport" $
      it "produces valid reports for interesting stores" $
        forAllValid $
          \wrc ->
            forAllValid $ \ha ->
              withInterestingStore $ \dc -> do
                nar <- produceWorkReport ha DontPrint dc wrc
                shouldBeValid nar
