{-# LANGUAGE TypeApplications #-}

module Smos.APISpec (spec) where

import Smos.API
import Smos.API.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @Username
  genValidSpec @Register
  genValidSpec @SyncFile
  genValidSpec @SyncRequest
  genValidSpec @SyncResponse
  genValidSpec @BackupInfo
  genValidSpec @UserInfo
