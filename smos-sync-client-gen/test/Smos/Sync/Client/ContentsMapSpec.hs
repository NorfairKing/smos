{-# LANGUAGE TypeApplications #-}

module Smos.Sync.Client.ContentsMapSpec
  ( spec
  ) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Validity
import Test.Validity.Optics

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
      forAllValid $ \cm -> forAllValid $ \bs -> forAll (mapWithNewPath cm bs) shouldBeValid
    describe "mapsWithDifferentContentsAtNewPath" $
      it "generates valid values" $
      forAllValid $ \cm -> forAll (mapsWithDifferentContentsAtNewPath cm) shouldBeValid
    describe "mapsWithDifferentContentsAtNewPath3" $
      it "generates valid values" $
      forAllValid $ \cm -> forAll (mapsWithDifferentContentsAtNewPath3 cm) shouldBeValid
    describe "changedContentsMap" $
      it "generates valid values" $
      forAllValid $ \cm -> forAll (changedContentsMap cm) shouldBeValid
    describe "changedMapsWithUnionOf" $
      it "generates valid values" $
      forAllValid $ \cm -> forAll (changedMapsWithUnionOf cm) shouldBeValid
    describe "mapWithAdditions" $
      it "generates valid values" $ forAllValid $ \cm -> forAll (mapWithAdditions cm) shouldBeValid
    describe "twoDistinctPathsThatFitAndTheirUnion" $
      it "generates valid values" $
      forAllValid $ \bs1 ->
        forAllValid $ \bs2 -> forAll (twoDistinctPathsThatFitAndTheirUnion bs1 bs2) shouldBeValid
    describe "twoDistinctPathsThatFitAndTheirUnionWith" $
      it "generates valid values" $
      forAllValid $ \m ->
        forAllValid $ \bs1 ->
          forAllValid $ \bs2 ->
            forAll (twoDistinctPathsThatFitAndTheirUnionWith m bs1 bs2) shouldBeValid
    describe "twoDistinctPathsThatFitAndTheirUnionsWith" $
      it "generates valid values" $
      forAllValid $ \m ->
        forAllValid $ \bs1 ->
          forAllValid $ \bs2 ->
            forAll (twoDistinctPathsThatFitAndTheirUnionsWith m bs1 bs2) shouldBeValid
    describe "disjunctContentsMap" $
      it "generates valid values" $
      forAllValid $ \cm -> forAll (disjunctContentsMap cm) shouldBeValid
    describe "mapWithDisjunctUnion" $
      it "generates valid values" $
      forAllValid $ \cm -> forAll (mapWithDisjunctUnion cm) shouldBeValid
    describe "twoChangedMapsAndTheirUnions" $
      it "generates valid values" $ forAll twoChangedMapsAndTheirUnions shouldBeValid
    describe "twoChangedMapsAndTheirUnionsWith" $
      it "generates valid values" $
      forAllValid $ \cm -> forAll (twoChangedMapsAndTheirUnionsWith cm) shouldBeValid
    describe "threeDisjunctMapsAndTheirUnions" $
      it "generates valid values" $ forAll threeDisjunctMapsAndTheirUnions shouldBeValid
    describe "empty" $ it "is valid" $ shouldBeValid CM.empty
    describe "singleton" $ it "produces valid contents maps" $ producesValidsOnValids2 CM.singleton
    describe "insert" $ it "produces valid contents maps" $ producesValidsOnValids3 CM.insert
    describe "union" $ it "produces valid contents maps" $ producesValidsOnValids2 CM.union
    describe "makeDirForest" $
      it "produces valid DirForests" $ producesValidsOnValids CM.makeDirForest
