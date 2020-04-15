{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.TimeBlock.Gen where

import Data.GenValidity
import Smos.Report.TimeBlock

instance GenValid TimeBlock where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance (GenUnchecked a, GenUnchecked b) => GenUnchecked (Block a b)

instance (GenValid a, GenValid b) => GenValid (Block a b) where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
