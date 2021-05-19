{-# LANGUAGE OverloadedStrings #-}

module Smos.Web.Server.Handler.AccountSpec (spec) where

import Smos.Web.Server.Foundation
import Smos.Web.Server.TestUtils
import Test.Syd
import Test.Syd.Yesod

spec :: Spec
spec = smosWebServerSpec $ do
  it "is possible to fetch AccountR and get a 200 response after loggin in" $ \yc ->
    withAnyFreshAccount_ yc $ do
      get AccountR
      statusIs 200
