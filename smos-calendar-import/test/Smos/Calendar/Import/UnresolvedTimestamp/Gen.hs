{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Calendar.Import.UnresolvedTimestamp.Gen where

import Data.GenValidity
import Data.GenValidity.Text ()
import Smos.Calendar.Import.Static.Gen ()
import Smos.Calendar.Import.TimeZone.Gen ()
import Smos.Calendar.Import.UnresolvedTimestamp
import Smos.Data.Gen ()

instance GenValid CalEndDuration where
  shrinkValid = shrinkValidStructurally
  genValid = genValidStructurally

instance GenValid CalTimestamp where
  shrinkValid = shrinkValidStructurally
  genValid = genValidStructurally

instance GenValid CalDateTime where
  shrinkValid = shrinkValidStructurally
  genValid = genValidStructurally
