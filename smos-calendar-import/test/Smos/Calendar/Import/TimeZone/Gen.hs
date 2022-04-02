{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Calendar.Import.TimeZone.Gen where

import Data.GenValidity
import Data.GenValidity.Text ()
import Smos.Calendar.Import.RecurrenceRule.Gen ()
import Smos.Calendar.Import.TimeZone
import Smos.Calendar.Import.UnresolvedTimestamp.Gen ()
import Smos.Data.Gen

instance GenValid TimeZoneHistory where
  shrinkValid = shrinkValidStructurally
  genValid = genValidStructurally

instance GenValid TimeZoneHistoryRule where
  shrinkValid = shrinkValidStructurally
  genValid =
    TimeZoneHistoryRule
      <$> genImpreciseLocalTime
      <*> genValid
      <*> genValid
      <*> genValid
      <*> genValid

instance GenValid UTCOffset where
  shrinkValid = shrinkValidStructurally
  genValid = genValidStructurally
