{-# LANGUAGE TypeApplications #-}

module Smos.Sync.APISpec
  ( spec
  ) where

import Smos.Sync.API
import Smos.Sync.API.Gen ()
import Test.Hspec
import Test.Validity
import Test.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @SyncFile
  jsonSpecOnValid @SyncFile
  genValidSpec @SyncRequest
  jsonSpecOnValid @SyncRequest
  genValidSpec @SyncResponse
  jsonSpecOnValid @SyncResponse
