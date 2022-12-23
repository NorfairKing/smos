{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Calendar.Import.RecurringEvent.Gen where

import Data.GenValidity
import Data.GenValidity.Text ()
import ICal.Gen ()
import ICal.Recurrence.Gen ()
import Smos.Calendar.Import.RecurringEvent
import Smos.Calendar.Import.Static.Gen ()
import Smos.Data.Gen ()

instance GenValid RecurringEvents where
  shrinkValid = shrinkValidStructurally
  genValid = genValidStructurally
