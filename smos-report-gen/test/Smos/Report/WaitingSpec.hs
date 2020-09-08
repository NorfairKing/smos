{-# LANGUAGE TypeApplications #-}

module Smos.Report.WaitingSpec where

import Smos.Report.Archive.Gen ()
import Smos.Report.Filter.Gen ()
import Smos.Report.TestUtils
import Smos.Report.Waiting
import Smos.Report.Waiting.Gen ()
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Validity
import Test.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @WaitingReport
  jsonSpecOnValid @WaitingReport
  genValidSpec @WaitingActionEntry
  jsonSpecOnValid @WaitingActionEntry
  modifyMaxSuccess (`div` 10) $ describe "produceWaitingReport" $ it "produces valid reports for interesting stores"
    $ forAllValid
    $ \mFilter ->
      forAllValid $ \ha ->
        withInterestingStore $ \dc -> do
          nar <- produceWaitingReport mFilter ha dc
          shouldBeValid nar
