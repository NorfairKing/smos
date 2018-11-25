{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Types.Gen where

import TestImport

import Smos.Types

instance GenUnchecked a => GenUnchecked (MStop a)

instance GenValid a => GenValid (MStop a)

instance GenUnchecked ActionName

instance GenValid ActionName where
    genValid = genValidStructurally
