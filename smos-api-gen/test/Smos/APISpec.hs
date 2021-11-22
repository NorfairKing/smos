{-# LANGUAGE TypeApplications #-}

module Smos.APISpec
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
  jsonSpec @Username
  genValidSpec @Register
  jsonSpec @Register
  genValidSpec @SyncFile
  jsonSpec @SyncFile
  genValidSpec @SyncRequest
  jsonSpec @SyncRequest
  genValidSpec @SyncResponse
  jsonSpec @SyncResponse
  genValidSpec @BackupInfo
  jsonSpec @BackupInfo
  genValidSpec @UserInfo
  jsonSpec @UserInfo
