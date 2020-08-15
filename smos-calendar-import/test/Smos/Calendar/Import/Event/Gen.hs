{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Calendar.Import.Event.Gen where

import Data.GenValidity
import Smos.Calendar.Import.Event
import Smos.Calendar.Import.Static.Gen ()
import Smos.Data.Gen ()

instance GenValid Events where
  shrinkValid = shrinkValidStructurally
  genValid = genValidStructurally

instance GenValid Event where
  shrinkValid = shrinkValidStructurally
  genValid = genValidStructurally
