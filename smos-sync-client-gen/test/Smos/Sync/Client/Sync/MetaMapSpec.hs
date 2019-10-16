{-# LANGUAGE TypeApplications #-}

module Smos.Sync.Client.Sync.MetaMapSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck
import Test.Validity

import qualified Data.Map as M
import Path

import qualified Smos.Sync.Client.MetaMap as MM
import Smos.Sync.Client.MetaMap (MetaMap(..))
import Smos.Sync.Client.MetaMap.Gen

spec :: Spec
spec = do
  genValidSpec @MetaMap
  describe "empty" $ it "is valid" $ shouldBeValid MM.empty
  describe "singleton" $ it "produces valid contents maps" $ producesValidsOnValids2 MM.singleton
  describe "insert" $ it "produces valid contents maps" $ producesValidsOnValids3 MM.insert
  describe "union" $ it "produces valid contents maps" $ producesValidsOnValids2 MM.union
  describe "unions" $ it "produces valid contents maps" $ producesValidsOnValids MM.unions
