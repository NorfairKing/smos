module Smos.ClientSpec
  ( spec,
  )
where

import Smos.API
import Smos.Client
import Smos.Data
import Test.Syd

spec :: Spec
spec = do
  describe "clientVersionCheck" $ do
    it "supports the current api version" $
      versionCheck oldestSupportedAPIVersion newestSupportedAPIVersion apiVersion `shouldBe` Supported
    it "says that the oldest supported api version is older than the current api version" $
      oldestSupportedAPIVersion `shouldSatisfy` (<= apiVersion)
    it "says that the newest supported api version is newer than the current api version" $
      newestSupportedAPIVersion `shouldSatisfy` (>= apiVersion)
