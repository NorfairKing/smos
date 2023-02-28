module Smos.Sizing.ReportSpec (spec) where

import Smos.Data.Gen ()
import Smos.Sizing.Command.Report
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "computeTotalRawDays" $ do
    it "produces valid results" $
      producesValid computeTotalRawDays
  describe "renderReport" $ do
    it "produces valid results" $
      producesValid renderReport
