{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Sync.Client.Sync.Gen where

import Data.GenValidity
import Pantry.SHA256
import Smos.API.Gen ()
import Smos.Sync.Client.Env
import Smos.Sync.Client.OptParse.Types

instance GenValid SHA256 where
  genValid = hashBytes <$> genValid
  shrinkValid _ = [] --No point in shrinking a hash, I think.

instance GenValid IgnoreFiles where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid ClientStore where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid SyncFileMeta where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
