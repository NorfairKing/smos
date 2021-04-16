{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.Handler.GetApiVersionSpec
  ( spec,
  )
where

import Smos.API as API
import Smos.Client
import Smos.Server.TestUtils
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec =
  serverSpec $
    describe "GetApiVersion" $
      it "works for any username and password" $ \cenv -> do
        receivedVersion <- testClient cenv clientGetApiVersion
        shouldBeValid apiVersion
        receivedVersion `shouldBe` API.apiVersion
