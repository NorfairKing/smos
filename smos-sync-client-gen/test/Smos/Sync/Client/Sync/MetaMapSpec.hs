{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Sync.Client.Sync.MetaMapSpec
  ( spec,
  )
where

import Path
import Smos.Sync.Client.MetaMap (MetaMap (..))
import qualified Smos.Sync.Client.MetaMap as MM
import Smos.Sync.Client.MetaMap.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @MetaMap
  describe "empty" $ it "is valid" $ shouldBeValid MM.empty
  describe "singleton" $ it "produces valid contents maps" $ producesValidsOnValids2 MM.singleton
  describe "fromListIgnoringCollisions" $ do
    it "produces valid contents maps" $ producesValidsOnValids MM.fromListIgnoringCollisions
    it "Remembers the longest paths it can for this example" $
      forAllValid $ \bs1 -> forAllValid $ \bs2 -> do
        let p1 = [relfile|foo|]
            p2 = [relfile|foo/bar|]
            list = [(p1, bs1), (p2, bs2)]
        MM.fromListIgnoringCollisions list `shouldBe` MM.singleton p2 bs2
        MM.fromListIgnoringCollisions (reverse list) `shouldBe` MM.singleton p2 bs2
