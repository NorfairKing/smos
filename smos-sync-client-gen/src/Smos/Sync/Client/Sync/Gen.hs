{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Sync.Client.Sync.Gen where

import Data.GenValidity
import Smos.API.Gen ()
import Smos.Sync.Client.Env
import Smos.Sync.Client.OptParse.Types

instance GenValid IgnoreFiles where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid ClientStore where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid SyncFileMeta where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
