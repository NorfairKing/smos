{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.Clock.Gen where

import Data.GenValidity
import Data.GenValidity.Path ()

import Smos.Data.Gen ()

import Smos.Report.Clock

import Smos.Report.Path.Gen ()
import Smos.Report.Period.Gen ()
import Smos.Report.TimeBlock.Gen ()

instance GenUnchecked DecimalClockResolution

instance GenValid DecimalClockResolution where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenUnchecked TemporalClockResolution

instance GenValid TemporalClockResolution where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenUnchecked ClockFormat

instance GenValid ClockFormat where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
