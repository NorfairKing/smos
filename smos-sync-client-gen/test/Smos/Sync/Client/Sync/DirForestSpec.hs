{-# LANGUAGE TypeApplications #-}

module Smos.Sync.Client.Sync.DirForestSpec
  ( spec
  ) where

import Test.Hspec
import Test.Validity

import qualified Smos.Sync.Client.DirForest as DF
import Smos.Sync.Client.DirForest (DirForest(..))
import Smos.Sync.Client.DirForest.Gen ()

spec :: Spec
spec = do
  genValidSpec @(DirForest Int)
  describe "makeDirForest" $
    it "produces valid DirForests" $ producesValidsOnValids (DF.makeDirForest @Int)
