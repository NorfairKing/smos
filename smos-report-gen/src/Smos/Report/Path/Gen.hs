{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.Path.Gen where

import Data.GenValidity
import Data.GenValidity.Path ()
import Smos.Report.Path

instance GenUnchecked RootedPath

instance GenValid RootedPath where
  genValid = genValidStructurally
