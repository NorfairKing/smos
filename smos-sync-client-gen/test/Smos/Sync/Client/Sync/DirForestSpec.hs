{-# LANGUAGE TypeApplications #-}

module Smos.Sync.Client.Sync.DirForestSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck
import Test.Validity

import qualified Data.Map as M
import Path

import Smos.Sync.Client.Contents
import qualified Smos.Sync.Client.DirForest as DF
import Smos.Sync.Client.DirForest (DirForest(..))
import Smos.Sync.Client.DirForest.Gen

spec :: Spec
spec = do
  genValidSpec @DirForest
  describe "makeDirForest" $
    it "produces valid DirForests" $ producesValidsOnValids DF.makeDirForest
