module Smos.Calendar.Import.RecurringEvent.Gen where

import Data.Char as Char
import Data.GenValidity
import Data.GenValidity.Text (genSingleLineText)
import qualified Data.Text as T
import Smos.Calendar.Import.RecurringEvent
import Test.QuickCheck

instance GenValid RecurringEvent where
  shrinkValid = shrinkValidStructurally
  genValid = genValidStructurally
