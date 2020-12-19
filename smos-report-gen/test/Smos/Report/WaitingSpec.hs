{-# LANGUAGE TypeApplications #-}

module Smos.Report.WaitingSpec where

import Smos.Report.Archive.Gen ()
import Smos.Report.Filter.Gen ()
import Smos.Report.ShouldPrint
import Smos.Report.TestUtils
import Smos.Report.Waiting
import Smos.Report.Waiting.Gen ()
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @WaitingReport
  jsonSpecOnValid @WaitingReport
  genValidSpec @WaitingEntry
  jsonSpecOnValid @WaitingEntry
  modifyMaxSuccess (`div` 10) $
    describe "produceWaitingReport" $
      it "produces valid reports for interesting stores" $
        forAllValid $
          \mFilter ->
            forAllValid $ \ha ->
              withInterestingStore $ \dc -> do
                nar <- produceWaitingReport mFilter ha DontPrint dc
                shouldBeValid nar
