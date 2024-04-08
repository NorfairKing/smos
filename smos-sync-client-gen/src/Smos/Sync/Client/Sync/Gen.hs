{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Sync.Client.Sync.Gen where

import Data.GenValidity
import Smos.API.Gen ()
import Smos.Sync.Client.Env

instance GenValid ClientStore where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid SyncFileMeta where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
