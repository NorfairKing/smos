{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.Period.Gen where

import Data.GenValidity
import Data.GenValidity.Time ()
import Smos.Report.Period

instance GenUnchecked Period

instance GenValid Period where
  genValid = genValidStructurally
