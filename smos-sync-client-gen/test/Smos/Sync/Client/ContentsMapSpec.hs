{-# LANGUAGE TypeApplications #-}

module Smos.Sync.Client.ContentsMapSpec
  ( spec
  ) where

import Test.Hspec
import Test.Validity
import Test.Validity.Optics

import qualified Smos.Sync.Client.ContentsMap as CM
import Smos.Sync.Client.ContentsMap (ContentsMap(..), contentsMapL)
import Smos.Sync.Client.ContentsMap.Gen

spec :: Spec
spec = do
  genValidSpec @ContentsMap
  describe "mapWithNewPath" $
    it "generates valid values" $
    forAllValid $ \cm -> forAllValid $ \bs -> genGeneratesValid (mapWithNewPath cm bs) (const [])
  describe "mapsWithDifferentContentsAtNewPath" $
    it "generates valid values" $
    forAllValid $ \cm -> genGeneratesValid (mapsWithDifferentContentsAtNewPath cm) (const [])
  describe "mapsWithDifferentContentsAtNewPath3" $
    it "generates valid values" $
    forAllValid $ \cm -> genGeneratesValid (mapsWithDifferentContentsAtNewPath3 cm) (const [])
  describe "changedContentsMap" $
    it "generates valid values" $
    forAllValid $ \cm -> genGeneratesValid (changedContentsMap cm) (const [])
  describe "changedMapsWithUnionOf" $
    it "generates valid values" $
    forAllValid $ \cm -> genGeneratesValid (changedMapsWithUnionOf cm) (const [])
  describe "mapWithAdditions" $
    it "generates valid values" $
    forAllValid $ \cm -> genGeneratesValid (mapWithAdditions cm) (const [])
  describe "twoDistinctPathsThatFitAndTheirUnion" $
    it "generates valid values" $
    forAllValid $ \bs1 ->
      forAllValid $ \bs2 ->
        genGeneratesValid (twoDistinctPathsThatFitAndTheirUnion bs1 bs2) (const [])
  describe "disjunctContentsMap" $
    it "generates valid values" $
    forAllValid $ \cm -> genGeneratesValid (disjunctContentsMap cm) (const [])
  describe "mapWithDisjunctUnion" $
    it "generates valid values" $
    forAllValid $ \cm -> genGeneratesValid (mapWithDisjunctUnion cm) (const [])
  describe "contentsMapL" $ lensSpecOnValid contentsMapL
  describe "empty" $ it "is valid" $ shouldBeValid CM.empty
  describe "singleton" $ it "produces valid contents maps" $ producesValidsOnValids2 CM.singleton
  describe "insert" $ it "produces valid contents maps" $ producesValidsOnValids3 CM.insert
  describe "union" $ it "produces valid contents maps" $ producesValidsOnValids2 CM.union
