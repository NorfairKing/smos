{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Calendar.Import.UnresolvedEvent.Gen where

import Data.GenValidity
import ICal.Gen ()
import ICal.Recurrence.Gen ()
import Smos.Calendar.Import.Static.Gen ()
import Smos.Calendar.Import.UnresolvedEvent
import Smos.Data.Gen ()

instance GenValid UnresolvedEvents where
  shrinkValid = shrinkValidStructurally
  genValid = genValidStructurally

instance GenValid UnresolvedEventGroup where
  shrinkValid = shrinkValidStructurally
  genValid = genValidStructurally
