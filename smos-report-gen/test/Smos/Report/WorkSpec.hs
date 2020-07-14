{-# LANGUAGE TypeApplications #-}

module Smos.Report.WorkSpec where

import Data.GenValidity.Path ()
import Smos.Data.Gen ()
import Smos.Report.Filter.Gen ()
import Smos.Report.Work
import Smos.Report.Work.Gen ()
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  genValidSpec @IntermediateWorkReport
  genValidSpec @WorkReport
  describe "makeIntermediateWorkReport"
    $ it "produces valid intermediate work reports"
    $ producesValidsOnValids3 makeIntermediateWorkReport
  describe "finishWorkReport" $ it "produces valid work reports" $ forAllValid $ \zt ->
    forAllValid $ \pn ->
      forAllValid $ \ms ->
        forAllValid $ \workReport ->
          shouldBeValid $ finishWorkReport zt pn ms workReport
