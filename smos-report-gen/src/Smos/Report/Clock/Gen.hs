{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.Clock.Gen where

import Data.GenValidity
import Data.GenValidity.Path ()

import Smos.Data.Gen ()

import Smos.Report.Clock

import Smos.Report.Path.Gen ()
import Smos.Report.Period.Gen ()
import Smos.Report.TimeBlock.Gen ()

instance GenUnchecked ClockResolution

instance GenValid ClockResolution where
    genValid = genValidStructurally

instance GenUnchecked ClockTime

instance GenValid ClockTime where
    genValid = genValidStructurally

instance GenUnchecked ClockTableEntry

instance GenValid ClockTableEntry where
    genValid = genValidStructurally
