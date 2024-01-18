{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.Free.Gen where

import Data.GenValidity
import Data.GenValidity.Path ()
import qualified Data.IntervalMap.Generic.Interval as IGI
import Data.IntervalMap.Generic.Lazy (IntervalMap)
import qualified Data.IntervalMap.Generic.Lazy as IM
import Data.Time
import Smos.Data.Gen ()
import Smos.Report.Free
import Test.QuickCheck

instance GenValid FreeReport

instance GenValid FreeMap

instance GenValid BusyMap where
  genValid =
    BusyMap
      <$> genIntervalMapOf
        ( (,)
            <$> genValid
            <*> genNonEmptyIntervalMapOf genValid
        )

instance GenValid Slot where
  genValid = do
    one <- genValid
    two <- genValid
    pure
      Slot
        { slotBegin = min one two,
          slotEnd = max one two
        }

genSmallSlot :: Gen Slot
genSmallSlot = sized $ \s -> do
  f <- choose (0, fromIntegral s) :: Gen Double
  let ndt = realToFrac f * nominalDay
  slotBegin <- genValid
  let slotEnd = addLocalTime ndt slotBegin
  pure Slot {..}

instance (GenValid a) => GenValid (Between a)

instance (IGI.Interval i k, Ord i, GenValid i, GenValid v) => GenValid (IntervalMap i v) where
  genValid = genIntervalMapOf genValid
  shrinkValid = fmap IM.fromList . shrinkValid . IM.toList

genNonEmptyIntervalMapOf :: (IGI.Interval i k, Ord i) => Gen (i, v) -> Gen (IntervalMap i v)
genNonEmptyIntervalMapOf gen = do
  (i, v) <- gen
  im <- genIntervalMapOf gen
  pure $ IM.insert i v im

genIntervalMapOf :: (IGI.Interval i k, Ord i) => Gen (i, v) -> Gen (IntervalMap i v)
genIntervalMapOf gen = IM.fromList <$> genListOf gen
