{-# LANGUAGE TypeApplications #-}

module Smos.Sync.Client.Sync.MetaMapSpec
  ( spec,
  )
where

import qualified Smos.Sync.Client.MetaMap as MM
import Smos.Sync.Client.MetaMap (MetaMap (..))
import Smos.Sync.Client.MetaMap.Gen ()
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  genValidSpec @MetaMap
  describe "empty" $ it "is valid" $ shouldBeValid MM.empty
  describe "singleton" $ it "produces valid contents maps" $ producesValidsOnValids2 MM.singleton
  describe "fromList" $ it "produces valid contents maps" $ producesValidsOnValids MM.fromList
  describe "insert" $ it "produces valid contents maps" $ producesValidsOnValids3 MM.insert
  describe "union" $ it "produces valid contents maps" $ producesValidsOnValids2 MM.union
  describe "unions" $ it "produces valid contents maps" $ producesValidsOnValids MM.unions
