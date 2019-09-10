{-# OPTIONS -fno-warn-orphans #-}

module Smos.Sync.API.Gen where

import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Mergeful ()
import Data.GenValidity.Path ()
import Data.GenValidity.UUID ()

import Smos.Sync.API

instance GenValid SyncFile where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

-- Currently still just a type synonym
-- instance GenValid SyncRequest where
--   genValid = genValidStructurally
--   shrinkValid = shrinkValidStructurally
instance GenValid SyncResponse where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
