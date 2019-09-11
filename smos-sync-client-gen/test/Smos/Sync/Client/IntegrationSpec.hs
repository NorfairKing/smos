{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Sync.Client.IntegrationSpec
  ( spec
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S

import Control.Monad

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Validity

import Servant.Client

import Path
import Path.IO

import Smos.Sync.Server.TestUtils

import Smos.Sync.Client.OptParse
import Smos.Sync.Client.Sync

import Smos.Sync.Client.Sync.Gen ()
import Smos.Sync.Client.TestUtils

spec :: Spec
spec =
  modifyMaxSuccess (* 10) $
  serverSpec $
  describe "syncSmosSyncClient" $ do
    describe "single client" $ do
      it "succesfully syncs an empty directory" $ \cenv ->
        withClient cenv $ \c -> do
          syncSmosSyncClient c
          assertClientContents c M.empty
      it "succesfully syncs a directory with one file" $ \cenv ->
        forAllValid $ \(rp, contents) ->
          withClient cenv $ \c -> do
            let m = M.singleton rp contents
            setupClientContents c m
            syncSmosSyncClient c
            assertClientContents c m
      it "succesfully syncs a directory with any number of files" $ \cenv ->
        forAllValid $ \m ->
          withClient cenv $ \c -> do
            setupClientContents c m
            syncSmosSyncClient c
            assertClientContents c m
    describe "two clients" $ do
      it "succesfully syncs empty directories" $ \cenv ->
        withClient cenv $ \c1 ->
          withClient cenv $ \c2 -> do
            syncSmosSyncClient c1
            syncSmosSyncClient c2
            assertClientContents c1 M.empty
            assertClientContents c2 M.empty
      describe "Additions only from one client" $ do
        it "succesfully syncs a file accross two clients" $ \cenv ->
          forAllValid $ \(rp, contents) ->
            withClient cenv $ \c1 ->
              withClient cenv $ \c2 -> do
                let m = M.singleton rp contents
                setupClientContents c1 m
                syncSmosSyncClient c1
                syncSmosSyncClient c2
                assertClientContents c1 m
                assertClientContents c2 m
        it "succesfully syncs any number of files accross two clients" $ \cenv ->
          forAllValid $ \m ->
            withClient cenv $ \c1 ->
              withClient cenv $ \c2 -> do
                setupClientContents c1 m
                syncSmosSyncClient c1
                syncSmosSyncClient c2
                assertClientContents c1 m
                assertClientContents c2 m
      describe "Additions only from both clients" $ do
        it "succesfully syncs a file accross two clients" $ \cenv ->
          forAllValid $ \(rp1, contents1) ->
            forAll (genValid `suchThat` (/= rp1)) $ \rp2 ->
              forAllValid $ \contents2 ->
                withClient cenv $ \c1 ->
                  withClient cenv $ \c2 -> do
                    let m1 = M.singleton rp1 contents1
                    let m2 = M.singleton rp2 contents2
                    let m = M.union m1 m2
                    setupClientContents c1 m1
                    setupClientContents c2 m2
                    syncSmosSyncClient c1
                    syncSmosSyncClient c2
                    assertClientContents c1 m
                    assertClientContents c2 m
        it "succesfully syncs any number of files accross two clients" $ \cenv ->
          forAllValid $ \m1 ->
            forAll (disjunctMap m1) $ \m2 ->
              withClient cenv $ \c1 ->
                withClient cenv $ \c2 -> do
                  let m = M.union m1 m2
                  setupClientContents c1 m1
                  setupClientContents c2 m2
                  syncSmosSyncClient c1
                  syncSmosSyncClient c2
                  assertClientContents c1 m
                  assertClientContents c2 m
