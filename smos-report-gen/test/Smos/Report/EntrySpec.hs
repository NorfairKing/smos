{-# LANGUAGE TypeApplications #-}

module Smos.Report.EntrySpec where

import Smos.Directory.Archive.Gen ()
import Smos.Directory.ShouldPrint
import Smos.Directory.TestUtils
import Smos.Report.Entry
import Smos.Report.Entry.Gen ()
import Smos.Report.Filter.Gen ()
import Smos.Report.Sorter.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @EntryReport
  modifyMaxSuccess (`div` 10) $
    describe "produceEntryReport" $
      it "produces valid reports for interesting stores" $
        forAllValid $
          \mFilter ->
            forAllValid $ \proj ->
              forAllValid $ \mSorter ->
                forAllValid $ \ha ->
                  withInterestingStore $ \dc -> do
                    er <- produceEntryReport mFilter ha DontPrint proj mSorter dc
                    shouldBeValid er
