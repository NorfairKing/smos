{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.Clock.Gen where

import Data.GenValidity
import Data.GenValidity.Path ()

import Smos.Data.Gen ()

import Smos.Report.Clock

instance GenUnchecked ClockPeriod

instance GenValid ClockPeriod

instance GenUnchecked ClockResolution

instance GenValid ClockResolution

instance GenUnchecked ClockBlock

instance GenValid ClockBlock

instance GenUnchecked ClockTime

instance GenValid ClockTime

instance GenUnchecked a => GenUnchecked (ClockTimeBlock a)

instance GenValid a => GenValid (ClockTimeBlock a)

instance GenUnchecked ClockTableBlock

instance GenValid ClockTableBlock

instance GenUnchecked ClockTableEntry

instance GenValid ClockTableEntry
