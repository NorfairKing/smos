{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Sync.Client.SaveSpec
  ( spec
  ) where

import qualified Data.Map as M


import Test.Hspec
import Test.Validity


import Smos.Sync.Client.TestUtils

import Smos.Sync.Client.Sync
import Smos.Sync.Client.Sync.Gen ()

spec :: Spec
spec =
  withTestDir $
  describe "saveContents" $ do
    it "puts a single file in the right place with the right contents" $ \d ->
      forAllValid $ \rf ->
        forAllValid $ \contents -> do
          let m = M.singleton rf contents
          saveContentsMap d m
          assertContents d m
    it "puts any number of files in the right place with the right contents" $ \d ->
      forAllValid $ \m -> do
        saveContentsMap d m
        assertContents d m
