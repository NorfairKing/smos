{-# LANGUAGE TypeApplications #-}

module Smos.Report.FreeSpec where

import Smos.Data
import Smos.Data.Gen ()
import Smos.Directory.Archive.Gen ()
import Smos.Directory.ShouldPrint
import Smos.Directory.TestUtils
import Smos.Report.Filter.Gen ()
import Smos.Report.Free
import Smos.Report.Free.Gen
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @(Between Entry)
  genValidSpec @Slot
  genValidSpec @BusyMap
  genValidSpec @FreeMap
  genValidSpec @FreeReport
  describe "entrySlot" $
    it "produces valid slots" $
      producesValid entrySlot
  describe "mkBusyMap" $
    it "produces valid busy maps" $
      producesValid mkBusyMap
  describe "busyMapToFreeMap" $ do
    it "produces valid busy maps" $
      producesValid3 busyMapToFreeMap
  describe "freeMapToFreeReport" $
    it "produces valid busy maps" $
      forAllShrink genSmallSlot shrinkValid $ \slot ->
        forAllValid $ \(mEarliest, mLatest) ->
          forAllValid $ \busyMap ->
            shouldBeValid $
              freeMapToFreeReport mEarliest mLatest $
                busyMapToFreeMap slot Nothing busyMap
  modifyMaxSuccess (`div` 10) $ do
    describe "produceBusyMap" $
      it "produces valid free maps for interesting stores" $
        forAllValid $ \ha ->
          withInterestingStore $ \dc -> do
            bm <- produceBusyMap ha DontPrint dc
            shouldBeValid bm
    describe "produceFreeMap" $
      it "produces valid free maps for interesting stores" $
        forAllValid $ \ha ->
          forAllValid $ \slot ->
            forAllValid $ \mMinimumTime ->
              withInterestingStore $ \dc -> do
                fm <- produceFreeMap ha DontPrint dc slot mMinimumTime
                shouldBeValid fm
    describe "produceFreeReport" $
      it "produces valid free report for interesting stores" $
        forAllValid $ \ha ->
          forAllValid $ \slot ->
            forAllValid $ \mMinimumTime ->
              forAllValid $ \mEarliest ->
                forAllValid $ \mLatest ->
                  withInterestingStore $ \dc -> do
                    fm <- produceFreeReport ha DontPrint dc slot mMinimumTime mEarliest mLatest
                    shouldBeValid fm
