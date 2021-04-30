{-# LANGUAGE OverloadedStrings #-}

module Smos.Web.Server.Handler.AuthSpec
  ( spec,
  )
where

import Smos.Web.Server.TestUtils
import Test.Syd
import Test.Syd.Yesod

spec :: Spec
spec = smosWebServerSpec $ do
  ydescribe "Register" $
    it "just works for a dummy username and password" $ \yc ->
      withAnyFreshAccount_ yc $ do
        pure ()
  ydescribe "Login" $
    it "just works for a dummy username and password" $ \yc ->
      withAnyFreshAccount yc $ \username password -> do
        loginTo username password
