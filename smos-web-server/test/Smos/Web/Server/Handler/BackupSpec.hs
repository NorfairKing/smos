{-# LANGUAGE OverloadedStrings #-}

module Smos.Web.Server.Handler.BackupSpec (spec) where

import Smos.Client
import Smos.Web.Server.Foundation
import Smos.Web.Server.TestUtils
import Test.Syd
import Test.Syd.Yesod

spec :: Spec
spec = smosWebServerSpec $ do
  let dummyUN = Username "dummy"
  let dummyPW = "dummy"
  yit "is possible to fetch BackupsR and get a 200 response after loggin in" $
    withFreshAccount dummyUN dummyPW $ do
      loginTo dummyUN dummyPW
      get BackupsR
      statusIs 200
