{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Calendar.Import.RecurringEvent.Gen where

import Data.GenValidity
import Data.GenValidity.Text ()
import Smos.Calendar.Import.RecurringEvent
import Smos.Data.Gen ()

instance GenValid CalEndDuration where
  shrinkValid = shrinkValidStructurally
  genValid = genValidStructurally

instance GenValid RecurringEvent where
  shrinkValid = shrinkValidStructurally
  genValid = genValidStructurally

instance GenValid CalTimestamp where
  shrinkValid = shrinkValidStructurally
  genValid = genValidStructurally

instance GenValid CalDateTime where
  shrinkValid = shrinkValidStructurally
  genValid = genValidStructurally

instance GenValid UTCOffset where
  shrinkValid = shrinkValidStructurally
  genValid = genValidStructurally
