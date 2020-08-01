module Smos.Calendar.Import.Event.Gen where

import Data.GenValidity
import Data.GenValidity.Text ()
import Smos.Calendar.Import.Event

instance GenValid Event where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
