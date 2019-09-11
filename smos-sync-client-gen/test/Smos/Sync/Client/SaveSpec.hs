{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Sync.Client.SaveSpec
  ( spec
  ) where

import GHC.Generics (Generic)

import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Mergeful as Mergeful
import qualified Data.Mergeful.Timed as Mergeful
import Data.Set (Set)
import qualified Data.Set as S
import Data.UUID

import Path
import Path.IO

import Control.Monad

import Servant.Client

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Validity
import Test.Validity.Aeson

import Smos.Sync.API

import Smos.Sync.Client.TestUtils
import Smos.Sync.Server.TestUtils

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
