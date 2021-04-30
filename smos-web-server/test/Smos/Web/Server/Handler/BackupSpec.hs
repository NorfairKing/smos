{-# LANGUAGE OverloadedStrings #-}

module Smos.Web.Server.Handler.BackupSpec (spec) where

import Smos.Web.Server.Foundation
import Smos.Web.Server.TestUtils
import Test.Syd
import Test.Syd.Yesod

spec :: Spec
spec = smosWebServerSpec $ do
  it "is possible to fetch BackupsR and get a 200 response after loggin in" $ \yc ->
    withAnyFreshAccount_ yc $ do
      get BackupsR
      statusIs 200
