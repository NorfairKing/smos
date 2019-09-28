{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Sync.Client.Sync.Gen where

import Data.GenValidity

import Smos.Sync.API.Gen ()

import Smos.Sync.Client.OptParse.Types
import Smos.Sync.Client.Sync

instance GenValid IgnoreFiles where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid ClientStore where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid ClientMetaData where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid SyncFileMeta where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
