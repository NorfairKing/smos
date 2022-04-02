{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Calendar.Import.UnresolvedTimestamp.Gen where

import Data.GenValidity
import Data.GenValidity.Text ()
import Smos.Calendar.Import.Static.Gen ()
import Smos.Calendar.Import.UnresolvedTimestamp
import Smos.Data.Gen
import Test.QuickCheck

instance GenValid CalRDate where
  shrinkValid = shrinkValidStructurally
  genValid = genValidStructurally

instance GenValid CalPeriod where
  shrinkValid = shrinkValidStructurally
  genValid = genValidStructurally

instance GenValid CalEndDuration where
  shrinkValid = shrinkValidStructurally
  genValid = genValidStructurally

instance GenValid CalTimestamp where
  shrinkValid = shrinkValidStructurally
  genValid = genValidStructurally

instance GenValid CalDateTime where
  shrinkValid = shrinkValidStructurally
  genValid =
    oneof
      [ Floating <$> genImpreciseLocalTime,
        UTC <$> genImpreciseUTCTime,
        Zoned <$> genImpreciseLocalTime <*> genValid
      ]

instance GenValid TimeZoneId where
  shrinkValid = shrinkValidStructurally
  genValid = genValidStructurally
