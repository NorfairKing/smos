module Smos.Data.VersionCheckSpec (spec) where

import Data.SemVer as Version
import Smos.Data (VersionCheck (..), versionCheck)
import Test.Syd

spec :: Spec
spec = do
  describe "versionCheck" $ do
    it "says that a version is supported if it's between the versions" $
      versionCheck
        (version 0 0 0 [] [])
        (version 1 0 0 [] [])
        (version 0 1 0 [] [])
        `shouldBe` Supported
    it "says that a version is supported if it's the same as the oldest" $
      versionCheck
        (version 0 0 0 [] [])
        (version 1 0 0 [] [])
        (version 0 0 0 [] [])
        `shouldBe` Supported
    it "says that a version is supported if it's the same as the newest" $
      versionCheck
        (version 0 0 0 [] [])
        (version 1 0 0 [] [])
        (version 1 0 0 [] [])
        `shouldBe` Supported
    it "says that a version is older than supported (minor) if the same as the oldest but a smaller minor" $
      versionCheck
        (version 1 1 0 [] [])
        (version 2 0 0 [] [])
        (version 1 0 0 [] [])
        `shouldBe` OlderThanSupportedMinor
    it "says that a version is newer than supported (minor) if the same as the newest but a greater minor" $
      versionCheck
        (version 0 0 0 [] [])
        (version 1 0 0 [] [])
        (version 1 1 0 [] [])
        `shouldBe` NewerThanSupportedMajor
    it "says that a version is older than supported if strictly smaller than the oldest" $
      versionCheck
        (version 1 0 0 [] [])
        (version 2 0 0 [] [])
        (version 0 0 0 [] [])
        `shouldBe` OlderThanSupportedMajor
    it "says that a version is newer than supported if strictly greater than the newest" $
      versionCheck
        (version 0 0 0 [] [])
        (version 1 0 0 [] [])
        (version 2 0 0 [] [])
        `shouldBe` NewerThanSupportedMajor
