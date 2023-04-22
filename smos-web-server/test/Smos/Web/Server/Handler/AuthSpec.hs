{-# LANGUAGE OverloadedStrings #-}

module Smos.Web.Server.Handler.AuthSpec
  ( spec,
  )
where

import Smos.Web.Server.TestUtils
import Test.Syd

spec :: Spec
spec = smosWebServerSpec $ do
  describe "Register" $
    it "just works for a dummy username and password" $ \yc ->
      withAnyFreshAccount_ yc $ do
        pure ()
  describe "Login" $
    it "just works for a dummy username and password" $ \yc ->
      withAnyFreshAccount yc $ \username password -> do
        loginTo username password
  describe "Logout" $
    it "just works for a dummy username and password" $ \yc ->
      withAnyFreshAccount yc $ \username password -> do
        loginTo username password
        logout
