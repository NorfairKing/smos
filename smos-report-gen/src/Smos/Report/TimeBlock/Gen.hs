{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.TimeBlock.Gen where

import Data.GenValidity

import Smos.Report.TimeBlock

instance GenUnchecked TimeBlock

instance GenValid TimeBlock where
    genValid = genValidStructurally
