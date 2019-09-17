module Smos.Sync.Client.IntegrationSpec
  ( spec
  ) where

import qualified Data.Map as M

import Test.Hspec
import Test.QuickCheck
import Test.Validity

import Smos.Sync.Server.TestUtils

import Smos.Sync.Client.OptParse.Types
import Smos.Sync.Client.Sync

import Smos.Sync.Client.Sync.Gen ()
import Smos.Sync.Client.TestUtils

spec :: Spec
spec =
  serverSpec $
  describe "syncSmosSyncClient" $ do
    describe "single client" $ do
      it "succesfully syncs an empty directory" $ \cenv ->
        withClient cenv $ \c -> do
          syncSmosSyncClient c
          assertClientContents c M.empty
      describe "addition" $ do
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
      describe "changes" $ do
        it "succesfully syncs a change" $ \cenv ->
          forAllValid $ \(rp, contents1, contents2) ->
            withClient cenv $ \c -> do
              let m1 = M.singleton rp contents1
              setupClientContents c m1
              syncSmosSyncClient c
              let m2 = M.singleton rp contents2
              setupClientContents c m2
              syncSmosSyncClient c
              assertClientContents c m2
        it "succesfully syncs a change from a set of files" $ \cenv ->
          forAllValid $ \(rp, contents1, contents2) ->
            forAllValid $ \m ->
              withClient cenv $ \c -> do
                let m1 = M.insert rp contents1 m
                setupClientContents c m1
                syncSmosSyncClient c
                let m2 = M.insert rp contents2 m
                setupClientContents c m2
                syncSmosSyncClient c
                assertClientContents c m2
        it "succesfully syncs a change of any number of files" $ \cenv ->
          forAllValid $ \m1 ->
            forAll (changedMap m1) $ \m2 ->
              withClient cenv $ \c -> do
                setupClientContents c m1
                syncSmosSyncClient c
                setupClientContents c m2
                syncSmosSyncClient c
                assertClientContents c m2
        it "succesfully syncs a change of any number of files from a set of files" $ \cenv ->
          forAllValid $ \m1' ->
            forAll (changedMap m1') $ \m2' ->
              forAllValid $ \m ->
                withClient cenv $ \c -> do
                  let m1 = M.union m1' m
                  let m2 = M.union m2' m
                  setupClientContents c m1
                  syncSmosSyncClient c
                  setupClientContents c m2
                  syncSmosSyncClient c
                  assertClientContents c m2
      describe "deletion" $ do
        it "succesfully syncs a single deletion" $ \cenv ->
          forAllValid $ \(rp, contents) ->
            withClient cenv $ \c -> do
              let m = M.singleton rp contents
              setupClientContents c m
              syncSmosSyncClient c
              let m' = M.empty
              setupClientContents c m'
              syncSmosSyncClient c
              assertClientContents c m'
        it "succesfully syncs a single deletion from a set of files" $ \cenv ->
          forAllValid $ \(rp, contents) ->
            forAllValid $ \m ->
              withClient cenv $ \c -> do
                let m' = M.insert rp contents m
                setupClientContents c m'
                syncSmosSyncClient c
                setupClientContents c m
                syncSmosSyncClient c
                assertClientContents c m
        it "succesfully syncs a deletion of any number of files" $ \cenv ->
          forAllValid $ \m ->
            withClient cenv $ \c -> do
              setupClientContents c m
              syncSmosSyncClient c
              let m' = M.empty
              setupClientContents c m'
              syncSmosSyncClient c
              assertClientContents c m'
        it "succesfully syncs a single deletion from a set of files" $ \cenv ->
          forAllValid $ \m1 ->
            forAllValid $ \m2 ->
              withClient cenv $ \c -> do
                let m = M.union m1 m2
                setupClientContents c m
                syncSmosSyncClient c
                setupClientContents c m1
                syncSmosSyncClient c
                assertClientContents c m1
    describe "two clients" $ do
      it "succesfully syncs empty directories" $ \cenv ->
        withClient cenv $ \c1 ->
          withClient cenv $ \c2 -> do
            syncSmosSyncClient c1
            syncSmosSyncClient c2
            assertClientContents c1 M.empty
            assertClientContents c2 M.empty
      describe "From one client" $ do
        describe "Additions only from one client" $ do
          it "succesfully syncs a file accross two clients" $ \cenv ->
            forAllValid $ \(rp, contents) ->
              withClient cenv $ \c1 ->
                withClient cenv $ \c2 -> do
                  let m = M.singleton rp contents
                  setupClientContents c1 m
                  fullySyncTwoClients c1 c2
                  assertClientContents c1 m
                  assertClientContents c2 m
          it "succesfully syncs any number of files accross two clients" $ \cenv ->
            forAllValid $ \m ->
              withClient cenv $ \c1 ->
                withClient cenv $ \c2 -> do
                  setupClientContents c1 m
                  fullySyncTwoClients c1 c2
                  assertClientContents c1 m
                  assertClientContents c2 m
        describe "changes" $ do
          it "succesfully syncs a single change" $ \cenv ->
            forAllValid $ \(rp, contents1, contents2) ->
              withClient cenv $ \c1 ->
                withClient cenv $ \c2 -> do
                  let m1 = M.singleton rp contents1
                  setupClientContents c1 m1
                  setupClientContents c1 m1
                  fullySyncTwoClients c1 c2
                  let m2 = M.singleton rp contents2
                  setupClientContents c1 m2
                  fullySyncTwoClients c1 c2
                  assertClientContents c1 m2
                  assertClientContents c2 m2
          it "succesfully syncs a change from a set of files" $ \cenv ->
            forAllValid $ \(rp, contents1, contents2) ->
              forAllValid $ \m ->
                withClient cenv $ \c1 ->
                  withClient cenv $ \c2 -> do
                    let m1 = M.insert rp contents1 m
                    setupClientContents c1 m1
                    setupClientContents c2 m1
                    fullySyncTwoClients c1 c2
                    let m2 = M.insert rp contents2 m
                    setupClientContents c1 m2
                    fullySyncTwoClients c1 c2
                    assertClientContents c1 m2
                    assertClientContents c2 m2
          it "succesfully syncs a change of any number of files" $ \cenv ->
            forAllValid $ \m1 ->
              forAll (changedMap m1) $ \m2 ->
                withClient cenv $ \c1 ->
                  withClient cenv $ \c2 -> do
                    setupClientContents c1 m1
                    setupClientContents c2 m1
                    fullySyncTwoClients c1 c2
                    setupClientContents c1 m2
                    fullySyncTwoClients c1 c2
                    assertClientContents c1 m2
                    assertClientContents c2 m2
          it "succesfully syncs a change of any number of files from a set of files" $ \cenv ->
            forAllValid $ \m1' ->
              forAll (changedMap m1') $ \m2' ->
                forAllValid $ \m ->
                  withClient cenv $ \c1 ->
                    withClient cenv $ \c2 -> do
                      let m1 = M.union m1' m
                      let m2 = M.union m2' m
                      setupClientContents c1 m1
                      setupClientContents c2 m1
                      fullySyncTwoClients c1 c2
                      setupClientContents c1 m2
                      fullySyncTwoClients c1 c2
                      assertClientContents c1 m2
                      assertClientContents c2 m2
        describe "Deletions" $ do
          it "succesfully syncs a single deletion" $ \cenv ->
            forAllValid $ \(rp, contents) ->
              withClient cenv $ \c1 ->
                withClient cenv $ \c2 -> do
                  let m = M.singleton rp contents
                  setupClientContents c1 m
                  setupClientContents c2 m
                  fullySyncTwoClients c1 c2
                  let m' = M.empty
                  setupClientContents c1 m'
                  fullySyncTwoClients c1 c2
                  assertClientContents c1 m'
                  assertClientContents c2 m'
          it "succesfully syncs a single deletion from a set of files" $ \cenv ->
            forAllValid $ \(rp, contents) ->
              forAllValid $ \m ->
                withClient cenv $ \c1 ->
                  withClient cenv $ \c2 -> do
                    let m' = M.insert rp contents m
                    setupClientContents c1 m'
                    setupClientContents c2 m'
                    fullySyncTwoClients c1 c2
                    setupClientContents c1 m
                    fullySyncTwoClients c1 c2
                    assertClientContents c1 m
                    assertClientContents c2 m
          it "succesfully syncs a deletion of any number of files" $ \cenv ->
            forAllValid $ \m ->
              withClient cenv $ \c1 ->
                withClient cenv $ \c2 -> do
                  setupClientContents c1 m
                  setupClientContents c2 m
                  fullySyncTwoClients c1 c2
                  let m' = M.empty
                  setupClientContents c1 m'
                  fullySyncTwoClients c1 c2
                  assertClientContents c1 m'
                  assertClientContents c2 m'
          it "succesfully syncs a deletion of any number of files from a set of files" $ \cenv ->
            forAllValid $ \m1 ->
              forAllValid $ \m2 ->
                withClient cenv $ \c1 ->
                  withClient cenv $ \c2 -> do
                    let m = M.union m1 m2
                    setupClientContents c1 m
                    setupClientContents c2 m
                    fullySyncTwoClients c1 c2
                    setupClientContents c1 m1
                    fullySyncTwoClients c1 c2
                    assertClientContents c1 m1
                    assertClientContents c2 m1
      describe "From both clients" $ do
        describe "Additions only" $ do
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
                      fullySyncTwoClients c1 c2
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
                    fullySyncTwoClients c1 c2
                    assertClientContents c1 m
                    assertClientContents c2 m
        describe "changes" $ do
          it "succesfully syncs a single change" $ \cenv ->
            forAllValid $ \(rp1, contents1a, contents1b) ->
              forAllValid $ \(rp2, contents2a, contents2b) ->
                withClient cenv $ \c1 ->
                  withClient cenv $ \c2 -> do
                    let m = M.insert rp1 contents1a $ M.singleton rp2 contents2a
                    setupClientContents c1 m
                    setupClientContents c1 m
                    fullySyncTwoClients c1 c2
                    let m1 = M.insert rp1 contents1b $ M.singleton rp2 contents2a
                    let m2 = M.insert rp1 contents1a $ M.singleton rp2 contents2b
                    setupClientContents c1 m1
                    setupClientContents c2 m2
                    fullySyncTwoClients c1 c2
                    let m' = M.insert rp1 contents1b $ M.singleton rp2 contents2b
                    assertClientContents c1 m'
                    assertClientContents c2 m'
          it "succesfully syncs a change from a set of files" $ \cenv ->
            forAllValid $ \(rp1, contents1a, contents1b) ->
              forAllValid $ \(rp2, contents2a, contents2b) ->
                forAllValid $ \m ->
                  withClient cenv $ \c1 ->
                    withClient cenv $ \c2 -> do
                      let ma = M.insert rp1 contents1a $ M.insert rp2 contents2a m
                      setupClientContents c1 ma
                      setupClientContents c1 ma
                      fullySyncTwoClients c1 c2
                      let m1 = M.insert rp1 contents1b $ M.insert rp2 contents2a m
                      let m2 = M.insert rp1 contents1a $ M.insert rp2 contents2b m
                      setupClientContents c1 m1
                      setupClientContents c2 m2
                      fullySyncTwoClients c1 c2
                      let mb = M.insert rp1 contents1b $ M.insert rp2 contents2b m
                      assertClientContents c1 mb
                      assertClientContents c2 mb
          it "succesfully syncs a change of any number of files" $ \cenv ->
            forAllValid $ \m1a ->
              forAll (changedMap m1a) $ \m1b ->
                forAllValid $ \m2a ->
                  forAll (changedMap m2a) $ \m2b ->
                    withClient cenv $ \c1 ->
                      withClient cenv $ \c2 -> do
                        let ma = M.union m1a m2a
                        setupClientContents c1 ma
                        setupClientContents c2 ma
                        fullySyncTwoClients c1 c2
                        let m1 = M.union m1b m2a
                        let m2 = M.union m1a m2b
                        setupClientContents c1 m1
                        setupClientContents c2 m2
                        fullySyncTwoClients c1 c2
                        let mb = M.union m1b m2b
                        assertClientContents c1 mb
                        assertClientContents c2 mb
          it "succesfully syncs a change of any number of files from a set of files" $ \cenv ->
            forAllValid $ \m1a ->
              forAll (changedMap m1a) $ \m1b ->
                forAllValid $ \m2a ->
                  forAll (changedMap m2a) $ \m2b ->
                  forAllValid $ \m ->
                    withClient cenv $ \c1 ->
                      withClient cenv $ \c2 -> do
                        let ma = M.unions [m1a, m2a,m]
                        setupClientContents c1 ma
                        setupClientContents c2 ma
                        fullySyncTwoClients c1 c2
                        let m1 = M.unions [m1b, m2a, m]
                        let m2 = M.unions [m1a, m2b, m]
                        setupClientContents c1 m1
                        setupClientContents c2 m2
                        fullySyncTwoClients c1 c2
                        let mb = M.unions [m1b, m2b,m]
                        assertClientContents c1 mb
                        assertClientContents c2 mb
        describe "Deletion" $ do
          it "succesfully syncs a single deletion" $ \cenv ->
            forAllValid $ \(rp1, contents1) ->
              forAll (genValid `suchThat` (/= rp1)) $ \rp2 ->
                forAllValid $ \contents2 ->
                  withClient cenv $ \c1 ->
                    withClient cenv $ \c2 -> do
                      let m = M.insert rp1 contents1 $ M.singleton rp2 contents2
                      setupClientContents c1 m
                      setupClientContents c2 m
                      fullySyncTwoClients c1 c2
                      let m1 = M.singleton rp2 contents2
                      let m2 = M.singleton rp1 contents1
                      setupClientContents c1 m1
                      setupClientContents c2 m2
                      fullySyncTwoClients c1 c2
                      let m' = M.empty
                      assertClientContents c1 m'
                      assertClientContents c2 m'
          it "succesfully syncs a single deletion from a set of files" $ \cenv ->
            forAllValid $ \(rp1, contents1) ->
              forAll (genValid `suchThat` (/= rp1)) $ \rp2 ->
                forAllValid $ \contents2 ->
                  forAllValid $ \m' ->
                    withClient cenv $ \c1 ->
                      withClient cenv $ \c2 -> do
                        let m = M.insert rp1 contents1 $ M.insert rp2 contents2 m'
                        setupClientContents c1 m
                        setupClientContents c2 m
                        fullySyncTwoClients c1 c2
                        let m1 = M.insert rp2 contents2 m'
                        let m2 = M.insert rp1 contents1 m'
                        setupClientContents c1 m1
                        setupClientContents c2 m2
                        fullySyncTwoClients c1 c2
                        assertClientContents c1 m'
                        assertClientContents c2 m'
          it "succesfully syncs a deletion of any number of files" $ \cenv ->
            forAllValid $ \m1 ->
              forAllValid $ \m2 ->
                withClient cenv $ \c1 ->
                  withClient cenv $ \c2 -> do
                    let m = M.union m1 m2
                    setupClientContents c1 m
                    setupClientContents c2 m
                    fullySyncTwoClients c1 c2
                    setupClientContents c1 m2
                    setupClientContents c2 m1
                    fullySyncTwoClients c1 c2
                    let m' = M.empty
                    assertClientContents c1 m'
                    assertClientContents c2 m'
          it "succesfully syncs a deletion of any number of files from a set of files" $ \cenv ->
            forAllValid $ \m1 ->
              forAllValid $ \m2 ->
                forAllValid $ \m3 ->
                  withClient cenv $ \c1 ->
                    withClient cenv $ \c2 -> do
                      let m = M.unions [m1, m2, m3]
                      setupClientContents c1 m
                      setupClientContents c2 m
                      fullySyncTwoClients c1 c2
                      setupClientContents c1 $ M.union m2 m3
                      setupClientContents c2 $ M.union m1 m3
                      fullySyncTwoClients c1 c2
                      assertClientContents c1 m3
                      assertClientContents c2 m3

fullySyncTwoClients :: SyncSettings -> SyncSettings -> IO ()
fullySyncTwoClients c1 c2 = fullySyncClients [c1, c2]

fullySyncClients :: [SyncSettings] -> IO ()
fullySyncClients cs = do
  let twice f = f >> f
  twice $ mapM_ syncSmosSyncClient cs
