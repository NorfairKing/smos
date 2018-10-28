{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.Period.Gen where

import Data.GenValidity

import Smos.Report.Period

instance GenUnchecked Period

instance GenValid Period where
    genValid = genValidStructurally
