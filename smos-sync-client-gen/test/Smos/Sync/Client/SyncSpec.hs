{-# LANGUAGE TypeApplications #-}

module Smos.Sync.Client.SyncSpec
  ( spec
  ) where

import Test.Hspec
import Test.Validity
import Test.Validity.Aeson

import Smos.Sync.Client.Sync
import Smos.Sync.Client.Sync.Gen ()

spec :: Spec
spec = do
  genValidSpec @ClientStore
  jsonSpecOnValid @ClientStore
  genValidSpec @ClientMetaData
  jsonSpecOnValid @ClientMetaData
  genValidSpec @SyncFileMeta
  jsonSpecOnValid @SyncFileMeta
