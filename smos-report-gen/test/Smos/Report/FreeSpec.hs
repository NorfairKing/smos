{-# LANGUAGE TypeApplications #-}

module Smos.Report.FreeSpec where

import qualified Data.IntervalMap.Generic.Lazy as IM
import qualified Data.Map as M
import Data.Time
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
  describe "mkBusyMap" $ do
    it "produces valid busy maps" $
      producesValid mkBusyMap
    it "produces an empty map when there are no entries" $
      mkBusyMap IM.empty `shouldBe` BusyMap IM.empty
  describe "busyMapToFreeMap" $ do
    it "produces valid busy maps" $
      producesValid3 busyMapToFreeMap
    it "produces a full free map" $
      forAllValid $ \today ->
        let slot =
              ( Slot
                  { slotBegin = LocalTime today midnight,
                    slotEnd = LocalTime (addDays 7 today) midnight
                  }
              )
         in busyMapToFreeMap
              slot
              Nothing
              (BusyMap IM.empty)
              `shouldBe` FreeMap (IM.singleton slot Free)
  describe "freeMapToFreeReport" $ do
    it "produces valid busy maps" $
      forAllShrink genSmallSlot shrinkValid $ \slot ->
        forAllValid $ \(mEarliest, mLatest) ->
          forAllValid $ \busyMap ->
            shouldBeValid $
              freeMapToFreeReport mEarliest mLatest $
                busyMapToFreeMap slot Nothing busyMap
    it "has a day for every day in the interval we care about" $
      forAllValid $ \today ->
        let slot =
              ( Slot
                  { slotBegin = LocalTime today midnight,
                    slotEnd = LocalTime (addDays 7 today) midnight
                  }
              )
         in freeMapToFreeReport
              Nothing
              Nothing
              (FreeMap (IM.singleton slot Free))
              `shouldBe` FreeReport
                ( M.fromList $ flip map [today .. addDays 6 today] $ \day ->
                    ( day,
                      FreeMap
                        { unFreeMap =
                            IM.singleton
                              ( Slot
                                  { slotBegin = LocalTime day midnight,
                                    slotEnd = LocalTime (addDays 1 day) midnight
                                  }
                              )
                              Free
                        }
                    )
                )

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
