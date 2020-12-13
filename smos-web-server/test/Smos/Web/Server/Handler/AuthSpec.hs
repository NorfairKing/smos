{-# LANGUAGE OverloadedStrings #-}

module Smos.Web.Server.Handler.AuthSpec
  ( spec,
  )
where

import Smos.Client
import Smos.Web.Server.TestUtils
import Test.Syd
import Yesod.Test

spec :: Spec
spec = smosWebServerSpec $ do
  let dummyUN = Username "dummy"
  let dummyPW = "dummy"
  ydescribe "Register" $
    yit "just works for a dummy username and password" $
      withFreshAccount dummyUN dummyPW $
        pure ()
  ydescribe "Login" $
    yit "just works for a dummy username and password" $
      withFreshAccount dummyUN dummyPW $
        loginTo dummyUN dummyPW
