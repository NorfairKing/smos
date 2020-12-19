{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Smos.Sync.Client.Sync.IntegrationSpec
  ( spec,
  )
where

import Path
import Servant.Client (ClientEnv)
import Smos.Server.TestUtils
import qualified Smos.Sync.Client.ContentsMap as CM
import Smos.Sync.Client.ContentsMap.Gen
import Smos.Sync.Client.Sync.Gen ()
import Smos.Sync.Client.TestUtils
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec =
  serverSpec $
    sequential $
      describe "testSyncSmosClient" $ do
        singleClientSpec
        twoClientSpec

singleClientSpec :: SpecWith ClientEnv
singleClientSpec =
  describe "single client" $ do
    it "succesfully syncs an empty directory" $ \cenv ->
      withNewRegisteredUser cenv $ \r ->
        withSyncClient cenv r $ \c -> do
          testSyncSmosClient c
          assertClientContents c CM.empty
    singleClientAdditionSpec
    singleClientChangeSpec
    singleClientDeletionSpec
    singleClientLargeSyncSpec

singleClientAdditionSpec :: SpecWith ClientEnv
singleClientAdditionSpec =
  describe "addition" $ do
    it "succesfully syncs a directory with one file" $ \cenv ->
      forAllValid $ \(rp, contents) ->
        withNewRegisteredUser cenv $ \r ->
          withSyncClient cenv r $ \c -> do
            let m = CM.singleton rp contents
            setupClientContents c m
            testSyncSmosClient c
            assertClientContents c m
    it "succesfully syncs a directory with any number of files" $ \cenv ->
      forAllValid $ \m ->
        withNewRegisteredUser cenv $ \r ->
          withSyncClient cenv r $ \c -> do
            setupClientContents c m
            testSyncSmosClient c
            assertClientContents c m
    it "does not remove a hidden file that is ignored" $ \cenv ->
      forAllHidden $ \rp ->
        forAllValid $ \contents ->
          withNewRegisteredUser cenv $ \r ->
            withHiddenFilesClient cenv r $ \c -> do
              let m = CM.singleton rp contents
              setupClientContents c m
              testSyncSmosClient c
              assertClientContents c m

singleClientChangeSpec :: SpecWith ClientEnv
singleClientChangeSpec =
  describe "changes" $ do
    it "succesfully syncs a change" $ \cenv ->
      forAllValid $ \(rp, contents1, contents2) ->
        withNewRegisteredUser cenv $ \r ->
          withSyncClient cenv r $ \c -> do
            let m1 = CM.singleton rp contents1
            setupClientContents c m1
            testSyncSmosClient c
            let m2 = CM.singleton rp contents2
            setupClientContents c m2
            testSyncSmosClient c
            assertClientContents c m2
    it "succesfully syncs a change from a set of files" $ \cenv ->
      forAllValid $ \m ->
        forAll (mapsWithDifferentContentsAtNewPath m) $ \(m1, m2) ->
          withNewRegisteredUser cenv $ \r ->
            withSyncClient cenv r $ \c -> do
              setupClientContents c m1
              testSyncSmosClient c
              setupClientContents c m2
              testSyncSmosClient c
              assertClientContents c m2
    it "succesfully syncs a change of any number of files" $ \cenv ->
      forAllValid $ \m1 ->
        forAll (changedContentsMap m1) $ \m2 ->
          withNewRegisteredUser cenv $ \r ->
            withSyncClient cenv r $ \c -> do
              setupClientContents c m1
              testSyncSmosClient c
              setupClientContents c m2
              testSyncSmosClient c
              assertClientContents c m2
    it "succesfully syncs a change of any number of files from a set of files" $ \cenv ->
      forAllValid $ \m ->
        forAll (changedMapsWithUnionOf m) $ \(m1, m2) ->
          withNewRegisteredUser cenv $ \r ->
            withSyncClient cenv r $ \c -> do
              setupClientContents c m1
              testSyncSmosClient c
              setupClientContents c m2
              testSyncSmosClient c
              assertClientContents c m2

singleClientDeletionSpec :: SpecWith ClientEnv
singleClientDeletionSpec =
  describe "deletion" $ do
    it "succesfully syncs a single deletion" $ \cenv ->
      forAllValid $ \(rp, contents) ->
        withNewRegisteredUser cenv $ \r ->
          withSyncClient cenv r $ \c -> do
            let m = CM.singleton rp contents
            setupClientContents c m
            testSyncSmosClient c
            let m' = CM.empty
            setupClientContents c m'
            testSyncSmosClient c
            assertClientContents c m'
    it "succesfully syncs a single deletion from a set of files" $ \cenv ->
      forAllValid $ \m ->
        forAllValid $ \contents ->
          forAll (mapWithNewPath m contents) $ \(_, m') ->
            withNewRegisteredUser cenv $ \r ->
              withSyncClient cenv r $ \c -> do
                setupClientContents c m'
                testSyncSmosClient c
                setupClientContents c m
                testSyncSmosClient c
                assertClientContents c m
    it "succesfully syncs a deletion of any number of files" $ \cenv ->
      forAllValid $ \m ->
        withNewRegisteredUser cenv $ \r ->
          withSyncClient cenv r $ \c -> do
            setupClientContents c m
            testSyncSmosClient c
            let m' = CM.empty
            setupClientContents c m'
            testSyncSmosClient c
            assertClientContents c m'
    it "succesfully syncs a single deletion from a set of files" $ \cenv ->
      forAllValid $ \m1 ->
        forAll (mapWithAdditions m1) $ \m ->
          withNewRegisteredUser cenv $ \r ->
            withSyncClient cenv r $ \c -> do
              setupClientContents c m
              testSyncSmosClient c
              setupClientContents c m1
              testSyncSmosClient c
              assertClientContents c m1

singleClientLargeSyncSpec :: SpecWith ClientEnv
singleClientLargeSyncSpec =
  xit "can sync a large store" $ \cenv ->
    forAll (sizedContentsMap 1000) $ \m ->
      withNewRegisteredUser cenv $ \r ->
        withSyncClient cenv r $ \c -> do
          setupClientContents c m
          testSyncSmosClient c
          assertClientContents c m

twoClientSpec :: SpecWith ClientEnv
twoClientSpec = do
  describe "two clients" $ do
    twoClientsEmptySpec
    twoClientsFromOneClientSpec
    twoClientsFromBothClientsSpec
    twoClientsFromBothClientsConcurrentlySpec
    twoClientsNastySyncSpec

twoClientsEmptySpec :: SpecWith ClientEnv
twoClientsEmptySpec =
  it "succesfully syncs empty directories" $ \cenv ->
    withNewRegisteredUser cenv $ \r ->
      withSyncClient cenv r $ \c1 ->
        withSyncClient cenv r $ \c2 -> do
          testSyncSmosClient c1
          testSyncSmosClient c2
          assertClientContents c1 CM.empty
          assertClientContents c2 CM.empty

twoClientsFromOneClientSpec :: SpecWith ClientEnv
twoClientsFromOneClientSpec = do
  describe "From one client" $ do
    describe "additions" $ do
      it "succesfully syncs a file accross two clients" $ \cenv ->
        forAllValid $ \(rp, contents) ->
          withNewRegisteredUser cenv $ \r ->
            withSyncClient cenv r $ \c1 ->
              withSyncClient cenv r $ \c2 -> do
                let m = CM.singleton rp contents
                setupClientContents c1 m
                fullySyncTwoClients c1 c2
                assertClientContents c1 m
                assertClientContents c2 m
      it "succesfully syncs any number of files accross two clients" $ \cenv ->
        forAllValid $ \m ->
          withNewRegisteredUser cenv $ \r ->
            withSyncClient cenv r $ \c1 ->
              withSyncClient cenv r $ \c2 -> do
                setupClientContents c1 m
                fullySyncTwoClients c1 c2
                assertClientContents c1 m
                assertClientContents c2 m
      it "does not sync over a hidden file that is ignored" $ \cenv ->
        forAllHidden $ \rp ->
          forAllValid $ \contents ->
            withNewRegisteredUser cenv $ \r ->
              withHiddenFilesClient cenv r $ \c1 ->
                withHiddenFilesClient cenv r $ \c2 -> do
                  let m = CM.singleton rp contents
                  setupClientContents c1 m
                  fullySyncTwoClients c1 c2
                  assertClientContents c1 m
                  assertClientContents c2 CM.empty
    describe "changes" $ do
      it "succesfully syncs a single change" $ \cenv ->
        forAllValid $ \(rp, contents1, contents2) ->
          withNewRegisteredUser cenv $ \r ->
            withSyncClient cenv r $ \c1 ->
              withSyncClient cenv r $ \c2 -> do
                let m1 = CM.singleton rp contents1
                setupClientContents c1 m1
                setupClientContents c2 m1
                fullySyncTwoClients c1 c2
                let m2 = CM.singleton rp contents2
                setupClientContents c1 m2
                fullySyncTwoClients c1 c2
                assertClientContents c1 m2
                assertClientContents c2 m2
      it "succesfully syncs a change from a set of files" $ \cenv ->
        forAllValid $ \m ->
          forAll (mapsWithDifferentContentsAtNewPath m) $ \(m1, m2) ->
            withNewRegisteredUser cenv $ \r ->
              withSyncClient cenv r $ \c1 ->
                withSyncClient cenv r $ \c2 -> do
                  setupClientContents c1 m1
                  setupClientContents c2 m1
                  fullySyncTwoClients c1 c2
                  setupClientContents c1 m2
                  fullySyncTwoClients c1 c2
                  assertClientContents c1 m2
                  assertClientContents c2 m2
      it "succesfully syncs a change of any number of files" $ \cenv ->
        forAllValid $ \m1 ->
          forAll (changedContentsMap m1) $ \m2 ->
            withNewRegisteredUser cenv $ \r ->
              withSyncClient cenv r $ \c1 ->
                withSyncClient cenv r $ \c2 -> do
                  setupClientContents c1 m1
                  setupClientContents c2 m1
                  fullySyncTwoClients c1 c2
                  setupClientContents c1 m2
                  fullySyncTwoClients c1 c2
                  assertClientContents c1 m2
                  assertClientContents c2 m2
      it "succesfully syncs a change of any number of files from a set of files" $ \cenv ->
        forAllValid $ \m ->
          forAll (changedMapsWithUnionOf m) $ \(m1, m2) ->
            withNewRegisteredUser cenv $ \r ->
              withSyncClient cenv r $ \c1 ->
                withSyncClient cenv r $ \c2 -> do
                  setupClientContents c1 m1
                  setupClientContents c2 m1
                  fullySyncTwoClients c1 c2
                  setupClientContents c1 m2
                  fullySyncTwoClients c1 c2
                  assertClientContents c1 m2
                  assertClientContents c2 m2
      it "does not sync changes in a hidden file that is ignored" $ \cenv ->
        forAllHidden $ \rp ->
          forAllValid $ \contents3 ->
            forAll (genValid `suchThat` (/= contents3)) $ \contents2 ->
              forAll (genValid `suchThat` (/= contents2) `suchThat` (/= contents3)) $ \contents1 ->
                withNewRegisteredUser cenv $ \r ->
                  withHiddenFilesClient cenv r $ \c1 ->
                    withHiddenFilesClient cenv r $ \c2 -> do
                      let m = CM.singleton rp contents3
                      setupClientContents c1 m
                      setupClientContents c2 m
                      fullySyncTwoClients c1 c2
                      let m1 = CM.singleton rp contents1
                      let m2 = CM.singleton rp contents2
                      setupClientContents c1 m1
                      setupClientContents c2 m2
                      fullySyncTwoClients c1 c2
                      assertClientContents c1 m1
                      assertClientContents c2 m2
    describe "Deletions" $ do
      it "succesfully syncs a single deletion" $ \cenv ->
        forAllValid $ \(rp, contents) ->
          withNewRegisteredUser cenv $ \r ->
            withSyncClient cenv r $ \c1 ->
              withSyncClient cenv r $ \c2 -> do
                let m = CM.singleton rp contents
                setupClientContents c1 m
                setupClientContents c2 m
                fullySyncTwoClients c1 c2
                let m' = CM.empty
                setupClientContents c1 m'
                fullySyncTwoClients c1 c2
                assertClientContents c1 m'
                assertClientContents c2 m'
      it "succesfully syncs a single deletion from a set of files" $ \cenv ->
        forAllValid $ \m ->
          forAllValid $ \contents ->
            forAll (mapWithNewPath m contents) $ \(_, m') ->
              withNewRegisteredUser cenv $ \r ->
                withSyncClient cenv r $ \c1 ->
                  withSyncClient cenv r $ \c2 -> do
                    setupClientContents c1 m'
                    setupClientContents c2 m'
                    fullySyncTwoClients c1 c2
                    setupClientContents c1 m
                    fullySyncTwoClients c1 c2
                    assertClientContents c1 m
                    assertClientContents c2 m
      it "succesfully syncs a deletion of any number of files" $ \cenv ->
        forAllValid $ \m ->
          withNewRegisteredUser cenv $ \r ->
            withSyncClient cenv r $ \c1 ->
              withSyncClient cenv r $ \c2 -> do
                setupClientContents c1 m
                setupClientContents c2 m
                fullySyncTwoClients c1 c2
                let m' = CM.empty
                setupClientContents c1 m'
                fullySyncTwoClients c1 c2
                assertClientContents c1 m'
                assertClientContents c2 m'
      it "succesfully syncs a deletion of any number of files from a set of files" $ \cenv ->
        forAllValid $ \m1 ->
          forAll (mapWithAdditions m1) $ \m ->
            withNewRegisteredUser cenv $ \r ->
              withSyncClient cenv r $ \c1 ->
                withSyncClient cenv r $ \c2 -> do
                  setupClientContents c1 m
                  setupClientContents c2 m
                  fullySyncTwoClients c1 c2
                  setupClientContents c1 m1
                  fullySyncTwoClients c1 c2
                  assertClientContents c1 m1
                  assertClientContents c2 m1
      it "does not sync a deletion of a hidden filed that is ignored" $ \cenv ->
        forAllHidden $ \rp ->
          forAllValid $ \contents ->
            withNewRegisteredUser cenv $ \r ->
              withHiddenFilesClient cenv r $ \c1 ->
                withHiddenFilesClient cenv r $ \c2 -> do
                  let m = CM.singleton rp contents
                  setupClientContents c1 m
                  setupClientContents c2 m
                  fullySyncTwoClients c1 c2
                  let m' = CM.empty
                  setupClientContents c1 m'
                  fullySyncTwoClients c1 c2
                  assertClientContents c1 m'
                  assertClientContents c2 m

twoClientsFromBothClientsSpec :: SpecWith ClientEnv
twoClientsFromBothClientsSpec = do
  describe "From both clients" $ do
    describe "Additions only" $ do
      it "succesfully syncs a file accross two clients" $ \cenv ->
        forAllValid $ \contents1 ->
          forAllValid $ \contents2 ->
            forAll (twoDistinctPathsThatFitAndTheirUnion contents1 contents2) $ \(rp1, rp2, m) ->
              withNewRegisteredUser cenv $ \r ->
                withSyncClient cenv r $ \c1 ->
                  withSyncClient cenv r $ \c2 -> do
                    let m1 = CM.singleton rp1 contents1
                    let m2 = CM.singleton rp2 contents2
                    setupClientContents c1 m1
                    setupClientContents c2 m2
                    fullySyncTwoClients c1 c2
                    assertClientContents c1 m
                    assertClientContents c2 m
      it "succesfully syncs any number of files accross two clients" $ \cenv ->
        forAllValid $ \m1 ->
          forAll (mapWithDisjunctUnion m1) $ \(m2, m) ->
            withNewRegisteredUser cenv $ \r ->
              withSyncClient cenv r $ \c1 ->
                withSyncClient cenv r $ \c2 -> do
                  setupClientContents c1 m1
                  setupClientContents c2 m2
                  fullySyncTwoClients c1 c2
                  assertClientContents c1 m
                  assertClientContents c2 m
      it "succesfully syncs two of the same additions at the same time" $ \cenv ->
        forAllValid $ \contents1 ->
          forAllValid $ \contents2 ->
            forAllValid $ \rp ->
              withNewRegisteredUser cenv $ \r ->
                withSyncClient cenv r $ \c1 ->
                  withSyncClient cenv r $ \c2 -> do
                    setupClientContents c1 CM.empty
                    setupClientContents c2 CM.empty
                    let m1 = CM.singleton rp contents1
                    let m2 = CM.singleton rp contents2
                    setupClientContents c1 m1
                    setupClientContents c2 m2
                    fullySyncTwoClients c1 c2
                    assertClientContents c1 m2 -- Not sure why it's m2 and not m1, but fine.
                    assertClientContents c2 m2
    describe "changes" $ do
      it "succesfully syncs a single change" $ \cenv ->
        forAll twoDistinctPathsThatFitAndTheirUnionFunc $ \(_, _, Hidden unionFunc) ->
          forAllValid $ \(contents1a, contents1b) ->
            forAllValid $ \(contents2a, contents2b) ->
              withNewRegisteredUser cenv $ \r ->
                withSyncClient cenv r $ \c1 ->
                  withSyncClient cenv r $ \c2 -> do
                    let m = unionFunc contents1a contents2a
                    setupClientContents c1 m
                    setupClientContents c1 m
                    fullySyncTwoClients c1 c2
                    let m1 = unionFunc contents1b contents2a
                    let m2 = unionFunc contents1a contents2b
                    setupClientContents c1 m1
                    setupClientContents c2 m2
                    fullySyncTwoClients c1 c2
                    let m' = unionFunc contents1b contents2b
                    assertClientContents c1 m'
                    assertClientContents c2 m'
      it "succesfully syncs a change from a set of files" $ \cenv ->
        forAllValid $ \m ->
          forAll (twoDistinctPathsThatFitAndTheirUnionWithFunc m) $ \(_, _, Hidden unionFunc) ->
            forAllValid $ \(contents1a, contents1b) ->
              forAllValid $ \(contents2a, contents2b) ->
                withNewRegisteredUser cenv $ \r ->
                  withSyncClient cenv r $ \c1 ->
                    withSyncClient cenv r $ \c2 -> do
                      let ma = unionFunc contents1a contents2a
                      setupClientContents c1 ma
                      setupClientContents c1 ma
                      fullySyncTwoClients c1 c2
                      let m1 = unionFunc contents1b contents2a
                      let m2 = unionFunc contents1a contents2b
                      setupClientContents c1 m1
                      setupClientContents c2 m2
                      fullySyncTwoClients c1 c2
                      let mb = unionFunc contents1b contents2b
                      assertClientContents c1 mb
                      assertClientContents c2 mb
      it "succesfully syncs a change of any number of files" $ \cenv ->
        forAll twoChangedMapsAndTheirUnions $ \(_, _, (ma, m1, m2, mb)) ->
          withNewRegisteredUser cenv $ \r ->
            withSyncClient cenv r $ \c1 ->
              withSyncClient cenv r $ \c2 -> do
                setupClientContents c1 ma
                setupClientContents c2 ma
                fullySyncTwoClients c1 c2
                setupClientContents c1 m1
                setupClientContents c2 m2
                fullySyncTwoClients c1 c2
                assertClientContents c1 mb
                assertClientContents c2 mb
      it "succesfully syncs a change of any number of files from a set of files" $ \cenv ->
        forAllValid $ \m ->
          forAll (twoChangedMapsAndTheirUnionsWith m) $ \(_, _, (ma, m1, m2, mb)) ->
            withNewRegisteredUser cenv $ \r ->
              withSyncClient cenv r $ \c1 ->
                withSyncClient cenv r $ \c2 -> do
                  setupClientContents c1 ma
                  setupClientContents c2 ma
                  fullySyncTwoClients c1 c2
                  setupClientContents c1 m1
                  setupClientContents c2 m2
                  fullySyncTwoClients c1 c2
                  assertClientContents c1 mb
                  assertClientContents c2 mb
    describe "Deletion" $ do
      it "succesfully syncs a single deletion" $ \cenv ->
        forAllValid $ \contents1 ->
          forAllValid $ \contents2 ->
            forAll (twoDistinctPathsThatFitAndTheirUnion contents1 contents2) $ \(rp1, rp2, m) ->
              withNewRegisteredUser cenv $ \r ->
                withSyncClient cenv r $ \c1 ->
                  withSyncClient cenv r $ \c2 -> do
                    setupClientContents c1 m
                    setupClientContents c2 m
                    fullySyncTwoClients c1 c2
                    let m1 = CM.singleton rp2 contents2
                    let m2 = CM.singleton rp1 contents1
                    setupClientContents c1 m1
                    setupClientContents c2 m2
                    fullySyncTwoClients c1 c2
                    let m' = CM.empty
                    assertClientContents c1 m'
                    assertClientContents c2 m'
      it "succesfully syncs a single deletion from a set of files" $ \cenv ->
        forAllValid $ \m' ->
          forAllValid $ \contents1 ->
            forAllValid $ \contents2 ->
              forAll (twoDistinctPathsThatFitAndTheirUnionsWith m' contents1 contents2) $ \(_, _, (m1, m2, m)) ->
                withNewRegisteredUser cenv $ \r ->
                  withSyncClient cenv r $ \c1 ->
                    withSyncClient cenv r $ \c2 -> do
                      setupClientContents c1 m
                      setupClientContents c2 m
                      fullySyncTwoClients c1 c2
                      setupClientContents c1 m1
                      setupClientContents c2 m2
                      fullySyncTwoClients c1 c2
                      assertClientContents c1 m'
                      assertClientContents c2 m'
      it "succesfully syncs a deletion of any number of files" $ \cenv ->
        forAllValid $ \m1 ->
          forAll (mapWithDisjunctUnion m1) $ \(m2, m) ->
            withNewRegisteredUser cenv $ \r ->
              withSyncClient cenv r $ \c1 ->
                withSyncClient cenv r $ \c2 -> do
                  setupClientContents c1 m
                  setupClientContents c2 m
                  fullySyncTwoClients c1 c2
                  setupClientContents c1 m2
                  setupClientContents c2 m1
                  fullySyncTwoClients c1 c2
                  let m' = CM.empty
                  assertClientContents c1 m'
                  assertClientContents c2 m'
      it "succesfully syncs a deletion of any number of files from a set of files" $ \cenv ->
        forAll threeDisjunctMapsAndTheirUnions $ \((_, _, m3), (_, m23, m13, m123)) ->
          withNewRegisteredUser cenv $ \r ->
            withSyncClient cenv r $ \c1 ->
              withSyncClient cenv r $ \c2 -> do
                setupClientContents c1 m123
                setupClientContents c2 m123
                fullySyncTwoClients c1 c2
                setupClientContents c1 m23
                setupClientContents c2 m13
                fullySyncTwoClients c1 c2
                assertClientContents c1 m3
                assertClientContents c2 m3
    describe "conflicts" $
      describe "both changed" $
        do
          it "succesfully syncs a single conflicting change" $ \cenv ->
            forAllValid $ \rp ->
              forAllValid $ \contents3 ->
                forAll (genValid `suchThat` (/= contents3)) $ \contents2 ->
                  forAll (genValid `suchThat` (/= contents2) `suchThat` (/= contents3)) $ \contents1 ->
                    withNewRegisteredUser cenv $ \r ->
                      withSyncClient cenv r $ \c1 ->
                        withSyncClient cenv r $ \c2 -> do
                          let m = CM.singleton rp contents3
                          setupClientContents c1 m
                          setupClientContents c2 m
                          fullySyncTwoClients c1 c2
                          let m1 = CM.singleton rp contents1
                          let m2 = CM.singleton rp contents2
                          setupClientContents c1 m1
                          setupClientContents c2 m2
                          fullySyncTwoClients c1 c2
                          let m' = CM.singleton rp contents1 -- client 1 synced first
                          assertClientContents c1 m'
                          assertClientContents c2 m'
          it "succesfully syncs a conflicting change from a set of files" $ \cenv ->
            forAllValid $ \m ->
              forAll (mapsWithDifferentContentsAtNewPath3 m) $ \(m1, m2, m3) ->
                withNewRegisteredUser cenv $ \r ->
                  withSyncClient cenv r $ \c1 ->
                    withSyncClient cenv r $ \c2 -> do
                      setupClientContents c1 m3
                      setupClientContents c2 m3
                      fullySyncTwoClients c1 c2
                      setupClientContents c1 m1
                      setupClientContents c2 m2
                      fullySyncTwoClients c1 c2
                      assertClientContents c1 m1
                      assertClientContents c2 m1

twoClientsFromBothClientsConcurrentlySpec :: SpecWith ClientEnv
twoClientsFromBothClientsConcurrentlySpec =
  describe "From two clients, concurrently" $ do
    it "succesfully syncs two of the same client concurrently" $ \cenv ->
      forAllValid $ \m1 ->
        withNewRegisteredUser cenv $ \r ->
          withSyncClient cenv r $ \c1 -> do
            setupClientContents c1 m1
            fullySyncTwoClientsConcurrently c1 c1
            cm1 <- readClientContents c1
            shouldBeValid cm1
    it "succesfully syncs two different clients concurrently" $ \cenv ->
      forAllValid $ \m1 ->
        forAllValid $ \m2 ->
          withNewRegisteredUser cenv $ \r ->
            withSyncClient cenv r $ \c1 ->
              withSyncClient cenv r $ \c2 -> do
                setupClientContents c1 m1
                setupClientContents c2 m2
                fullySyncTwoClientsConcurrently c1 c2
                cm1 <- readClientContents c1
                cm2 <- readClientContents c2
                cm1 `shouldBe` cm2

twoClientsNastySyncSpec :: SpecWith ClientEnv
twoClientsNastySyncSpec =
  -- It's ambiguous what should happen here.
  -- We opt for the "whatever reaches the server first, wins"
  describe "Nasty sync" $ do
    it "does not go horribly wrong in the nasty situation where clients add foo and foo/bar respectively" $ \cenv ->
      forAllValid $ \bs1 ->
        forAllValid $ \bs2 ->
          withNewRegisteredUser cenv $ \r ->
            withSyncClient cenv r $ \c1 ->
              withSyncClient cenv r $ \c2 -> do
                let cm1 = CM.singleton [relfile|foo|] bs1
                    cm2 = CM.singleton [relfile|foo/bar|] bs2
                setupClientContents c1 cm1
                setupClientContents c2 cm2
                fullySyncTwoClients c1 c2
                cm1' <- readClientContents c1
                cm2' <- readClientContents c2
                cm1' `shouldBe` cm1
                cm2' `shouldBe` cm1
    it "does not go horribly wrong in the nasty situation where clients add foo and foo/bar/quux respectively" $ \cenv ->
      forAllValid $ \bs1 ->
        forAllValid $ \bs2 ->
          withNewRegisteredUser cenv $ \r ->
            withSyncClient cenv r $ \c1 ->
              withSyncClient cenv r $ \c2 -> do
                let cm1 = CM.singleton [relfile|foo|] bs1
                    cm2 = CM.singleton [relfile|foo/bar/quux|] bs2
                setupClientContents c1 cm1
                setupClientContents c2 cm2
                fullySyncTwoClients c1 c2
                cm1' <- readClientContents c1
                cm2' <- readClientContents c2
                cm1' `shouldBe` cm1
                cm2' `shouldBe` cm1
    it "does not go horribly wrong in the nasty situation where clients add foo and foo/bar respectively, backwards" $ \cenv ->
      forAllValid $ \bs1 ->
        forAllValid $ \bs2 ->
          withNewRegisteredUser cenv $ \r ->
            withSyncClient cenv r $ \c1 ->
              withSyncClient cenv r $ \c2 -> do
                let cm1 = CM.singleton [relfile|foo/bar|] bs2
                    cm2 = CM.singleton [relfile|foo|] bs1
                setupClientContents c1 cm1
                setupClientContents c2 cm2
                fullySyncTwoClients c1 c2
                cm1' <- readClientContents c1
                cm2' <- readClientContents c2
                cm1' `shouldBe` cm1
                cm2' `shouldBe` cm1
    it "does not go horribly wrong in the nasty situation where clients add foo and foo/bar/quux respectively, backwards" $ \cenv ->
      forAllValid $ \bs1 ->
        forAllValid $ \bs2 ->
          withNewRegisteredUser cenv $ \r ->
            withSyncClient cenv r $ \c1 ->
              withSyncClient cenv r $ \c2 -> do
                let cm1 = CM.singleton [relfile|foo/bar/quux|] bs2
                    cm2 = CM.singleton [relfile|foo|] bs1
                setupClientContents c1 cm1
                setupClientContents c2 cm2
                fullySyncTwoClients c1 c2
                cm1' <- readClientContents c1
                cm2' <- readClientContents c2
                cm1' `shouldBe` cm1
                cm2' `shouldBe` cm1
