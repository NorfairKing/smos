{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.Clock.Gen where

import Data.GenValidity
import Data.GenValidity.Path ()
import Smos.Data.Gen ()
import Smos.Report.Clock
import Smos.Report.Period.Gen ()
import Smos.Report.TimeBlock.Gen ()

instance GenValid DecimalClockResolution where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid TemporalClockResolution where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid ClockFormat where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
