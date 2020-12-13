{-# LANGUAGE TypeApplications #-}

module Smos.Sync.Client.Sync.DirForestSpec
  ( spec,
  )
where

import Smos.Sync.Client.DirForest (DirForest (..))
import qualified Smos.Sync.Client.DirForest as DF
import Smos.Sync.Client.DirForest.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @(DirForest Int)
  describe "makeDirForest" $
    it "produces valid DirForests" $
      producesValidsOnValids (DF.makeDirForest @Int)
