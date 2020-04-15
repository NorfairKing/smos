{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Types.Gen where

import Smos.Types
import TestImport

instance GenUnchecked a => GenUnchecked (MStop a)

instance GenValid a => GenValid (MStop a) where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid ActionName where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
