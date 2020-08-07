{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Calendar.Import.UnresolvedEvent.Gen where

import Data.GenValidity
import Smos.Calendar.Import.Static.Gen ()
import Smos.Calendar.Import.TimeZone.Gen ()
import Smos.Calendar.Import.UnresolvedEvent
import Smos.Calendar.Import.UnresolvedTimestamp.Gen ()
import Smos.Data.Gen ()

instance GenValid UnresolvedEvents where
  shrinkValid = shrinkValidStructurally
  genValid = genValidStructurally

instance GenValid UnresolvedEventGroup where
  shrinkValid = shrinkValidStructurally
  genValid = genValidStructurally

instance GenValid UnresolvedEvent where
  shrinkValid = shrinkValidStructurally
  genValid = genValidStructurally
