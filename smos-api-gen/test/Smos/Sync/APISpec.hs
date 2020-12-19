{-# LANGUAGE TypeApplications #-}

module Smos.Sync.APISpec
  ( spec,
  )
where

import Smos.API
import Smos.API.Gen ()
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  genValidSpec @Username
  jsonSpecOnValid @Username
  genValidSpec @Register
  jsonSpecOnValid @Register
  genValidSpec @SyncFile
  jsonSpecOnValid @SyncFile
  genValidSpec @SyncRequest
  jsonSpecOnValid @SyncRequest
  genValidSpec @SyncResponse
  jsonSpecOnValid @SyncResponse
