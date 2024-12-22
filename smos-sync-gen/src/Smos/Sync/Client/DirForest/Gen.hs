{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Sync.Client.DirForest.Gen where

import Data.GenValidity
import Smos.API.Gen ()
import Smos.Sync.Client.DirForest

instance (GenValid a) => GenValid (DirForest a) where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance (GenValid a) => GenValid (DirOrFile a) where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
