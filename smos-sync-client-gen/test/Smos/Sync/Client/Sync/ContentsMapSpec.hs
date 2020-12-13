{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Sync.Client.Sync.ContentsMapSpec
  ( spec,
  )
where

import qualified Data.Map as M
import Path
import Smos.Sync.Client.Contents
import Smos.Sync.Client.ContentsMap (ContentsMap (..))
import qualified Smos.Sync.Client.ContentsMap as CM
import Smos.Sync.Client.ContentsMap.Gen
import Test.Syd

import Test.QuickCheck
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @ContentsMap
  describe "hideFile" $ do
    it "produces valid paths" $ forAllValid $ shouldBeValid . hideFile
    it "hides a file" $ forAllValid $ isHidden . hideFile
  describe "hideDir" $ do
    it "produces valid paths" $ forAllValid $ shouldBeValid . hideDir
    it "hides a dir" $ forAllValid $ \(d, f) -> isHidden $ hideDir d </> f
  describe "generators" $
    modifyMaxSuccess (`div` 10) $ do
      describe "genHiddenFile" $ it "generates hidden files" $ forAll genHiddenFile isHidden
      describe "mapWithNewPath" $
        it "generates valid values" $
          forAllValid $
            \cm -> forAllValid $ \bs -> genGeneratesValid (mapWithNewPath cm bs)
      describe "mapWithNewHiddenPath" $ do
        it "generates valid values" $
          forAllValid $
            \cm -> forAllValid $ \bs -> genGeneratesValid (mapWithNewHiddenPath cm bs)
        it "generates a contentsmap where at least one path is hidden" $
          forAllValid $
            \cm ->
              forAllValid $ \bs ->
                forAll (mapWithNewHiddenPath cm bs) $ \(hp, m) ->
                  isHidden hp && not (M.null $ M.filterWithKey (\p _ -> isHidden p) $ CM.contentsMapFiles m)
      describe "mapsWithDifferentContentsAtNewPath" $
        it "generates valid values" $
          forAllValid $
            \cm -> genGeneratesValid (mapsWithDifferentContentsAtNewPath cm)
      describe "mapsWithDifferentContentsAtNewPath3" $
        it "generates valid values" $
          forAllValid $
            \cm -> genGeneratesValid (mapsWithDifferentContentsAtNewPath3 cm)
      describe "changedContentsMap" $
        it "generates valid values" $
          forAllValid $
            \cm -> genGeneratesValid (changedContentsMap cm)
      describe "changedMapsWithUnionOf" $
        it "generates valid values" $
          forAllValid $
            \cm -> genGeneratesValid (changedMapsWithUnionOf cm)
      describe "mapWithAdditions" $
        it "generates valid values" $
          forAllValid $
            \cm -> genGeneratesValid (mapWithAdditions cm)
      describe "mapWithHiddenAdditions" $ do
        it "generates valid values" $ forAllValid $ \cm -> genGeneratesValid (mapWithHiddenAdditions cm)
        it "generates a contentsmap where at least one path is hidden" $
          forAllValid $
            \cm ->
              forAll (mapWithHiddenAdditions cm) $ \m ->
                not (M.null $ M.filterWithKey (\p _ -> isHidden p) $ CM.contentsMapFiles m)
      describe "twoDistinctPathsThatFitAndTheirUnion" $
        it "generates valid values" $
          forAllValid $
            \bs1 ->
              forAllValid $ \bs2 -> genGeneratesValid (twoDistinctPathsThatFitAndTheirUnion bs1 bs2)
      describe "twoDistinctPathsThatFitAndTheirUnionWith" $
        it "generates valid values" $
          forAllValid $
            \m ->
              forAllValid $ \bs1 ->
                forAllValid $ \bs2 -> genGeneratesValid (twoDistinctPathsThatFitAndTheirUnionWith m bs1 bs2)
      describe "twoDistinctPathsThatFitAndTheirUnionsWith" $
        it "generates valid values" $
          forAllValid $
            \m ->
              forAllValid $ \bs1 ->
                forAllValid $ \bs2 ->
                  genGeneratesValid (twoDistinctPathsThatFitAndTheirUnionsWith m bs1 bs2)
      describe "disjunctContentsMap" $
        it "generates valid values" $
          forAllValid $
            \cm -> genGeneratesValid (disjunctContentsMap cm)
      describe "mapWithDisjunctUnion" $
        it "generates valid values" $
          forAllValid $
            \cm -> genGeneratesValid (mapWithDisjunctUnion cm)
      describe "twoChangedMapsAndTheirUnions" $
        it "generates valid values" $
          genGeneratesValid twoChangedMapsAndTheirUnions
      describe "twoChangedMapsAndTheirUnionsWith" $
        it "generates valid values" $
          forAllValid $
            \cm -> genGeneratesValid (twoChangedMapsAndTheirUnionsWith cm)
      describe "threeDisjunctMapsAndTheirUnions" $
        it "generates valid values" $
          genGeneratesValid threeDisjunctMapsAndTheirUnions
  describe "empty" $ it "is valid" $ shouldBeValid CM.empty
  describe "singleton" $ it "produces valid contents maps" $ producesValidsOnValids2 CM.singleton
  describe "insert" $ it "produces valid contents maps" $ producesValidsOnValids3 CM.insert
  describe "fromListIgnoringCollisions" $ do
    it "produces valid content maps" $ producesValidsOnValids CM.fromListIgnoringCollisions
    it "Remembers the longest paths it can for this example" $
      forAllValid $ \bs1 -> forAllValid $ \bs2 -> do
        let p1 = [relfile|foo|]
            p2 = [relfile|foo/bar|]
            list = [(p1, bs1), (p2, bs2)]
        CM.fromListIgnoringCollisions list `shouldBe` CM.singleton p2 bs2
        CM.fromListIgnoringCollisions (reverse list) `shouldBe` CM.singleton p2 bs2
  describe "union" $ it "produces valid contents maps" $ producesValidsOnValids2 CM.union
  describe "unions" $ it "produces valid contents maps" $ producesValidsOnValids CM.unions
