{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.E2E.VersionSpec (spec) where

import Servant.Client
import Smos.Client
import Smos.Data
import Test.Syd

spec :: TestDef '[ClientEnv] ()
spec = do
  describe "Version" $ do
    itWithOuter "is compatible with the server version" $ \cenv -> do
      (serverVersion, check) <- runClientOrDie cenv clientVersionCheck
      context (show serverVersion) $ check `shouldBe` Supported
