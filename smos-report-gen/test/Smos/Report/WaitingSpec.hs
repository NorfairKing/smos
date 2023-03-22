{-# LANGUAGE TypeApplications #-}

module Smos.Report.WaitingSpec where

import Smos.Directory.Archive.Gen ()
import Smos.Directory.ShouldPrint
import Smos.Directory.TestUtils
import Smos.Report.Filter.Gen ()
import Smos.Report.Waiting
import Smos.Report.Waiting.Gen ()
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @WaitingReport
  jsonSpec @WaitingReport
  genValidSpec @WaitingEntry
  jsonSpec @WaitingEntry
  describe "finishWaitingReport" $ it "produces valid reports" $ producesValid finishWaitingReport
  modifyMaxSuccess (`div` 10) $
    describe "produceWaitingReport" $
      it "produces valid reports for interesting stores" $
        forAllValid $
          \mFilter ->
            forAllValid $ \ha ->
              withInterestingStore $ \dc -> do
                wr <- produceWaitingReport mFilter ha DontPrint dc
                shouldBeValid wr
