{-# LANGUAGE TypeApplications #-}

module Smos.Sync.Client.ContentsMapSpec
  ( spec
  ) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Validity

import qualified Smos.Sync.Client.ContentsMap as CM
import Smos.Sync.Client.ContentsMap (ContentsMap(..))
import Smos.Sync.Client.ContentsMap.Gen

spec :: Spec
spec =
  modifyMaxSize (* 10) $
  modifyMaxSuccess (* 10) $ do
    genValidSpec @ContentsMap
    describe "mapWithNewPath" $
      it "generates valid values" $
      forAllValid $ \cm -> forAllValid $ \bs -> genGeneratesValid (mapWithNewPath cm bs)
    describe "mapsWithDifferentContentsAtNewPath" $
      it "generates valid values" $
      forAllValid $ \cm -> genGeneratesValid (mapsWithDifferentContentsAtNewPath cm)
    describe "mapsWithDifferentContentsAtNewPath3" $
      it "generates valid values" $
      forAllValid $ \cm -> genGeneratesValid (mapsWithDifferentContentsAtNewPath3 cm)
    describe "changedContentsMap" $
      it "generates valid values" $ forAllValid $ \cm -> genGeneratesValid (changedContentsMap cm)
    describe "changedMapsWithUnionOf" $
      it "generates valid values" $
      forAllValid $ \cm -> genGeneratesValid (changedMapsWithUnionOf cm)
    describe "mapWithAdditions" $
      it "generates valid values" $ forAllValid $ \cm -> genGeneratesValid (mapWithAdditions cm)
    describe "twoDistinctPathsThatFitAndTheirUnion" $
      it "generates valid values" $
      forAllValid $ \bs1 ->
        forAllValid $ \bs2 -> genGeneratesValid (twoDistinctPathsThatFitAndTheirUnion bs1 bs2)
    describe "twoDistinctPathsThatFitAndTheirUnionWith" $
      it "generates valid values" $
      forAllValid $ \m ->
        forAllValid $ \bs1 ->
          forAllValid $ \bs2 ->
            genGeneratesValid (twoDistinctPathsThatFitAndTheirUnionWith m bs1 bs2)
    describe "twoDistinctPathsThatFitAndTheirUnionsWith" $
      it "generates valid values" $
      forAllValid $ \m ->
        forAllValid $ \bs1 ->
          forAllValid $ \bs2 ->
            genGeneratesValid (twoDistinctPathsThatFitAndTheirUnionsWith m bs1 bs2)
    describe "disjunctContentsMap" $
      it "generates valid values" $ forAllValid $ \cm -> genGeneratesValid (disjunctContentsMap cm)
    describe "mapWithDisjunctUnion" $
      it "generates valid values" $ forAllValid $ \cm -> genGeneratesValid (mapWithDisjunctUnion cm)
    describe "twoChangedMapsAndTheirUnions" $
      it "generates valid values" $ genGeneratesValid twoChangedMapsAndTheirUnions
    describe "twoChangedMapsAndTheirUnionsWith" $
      it "generates valid values" $
      forAllValid $ \cm -> genGeneratesValid (twoChangedMapsAndTheirUnionsWith cm)
    describe "threeDisjunctMapsAndTheirUnions" $
      it "generates valid values" $ genGeneratesValid threeDisjunctMapsAndTheirUnions
    describe "empty" $ it "is valid" $ shouldBeValid CM.empty
    describe "singleton" $ it "produces valid contents maps" $ producesValidsOnValids2 CM.singleton
    describe "insert" $ it "produces valid contents maps" $ producesValidsOnValids3 CM.insert
    describe "union" $ it "produces valid contents maps" $ producesValidsOnValids2 CM.union
    describe "unions" $ it "produces valid contents maps" $ producesValidsOnValids CM.unions
    describe "makeDirForest" $
      it "produces valid DirForests" $ producesValidsOnValids CM.makeDirForest
