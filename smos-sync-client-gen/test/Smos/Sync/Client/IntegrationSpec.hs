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
import Test.Validity

import Servant.Client

import Path
import Path.IO

import Smos.Sync.Client.Sync.Gen ()
import Smos.Sync.Server.TestUtils

import Smos.Sync.Client.OptParse
import Smos.Sync.Client.Sync

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
            forAllValid $ \(rp2, contents2) ->
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
            forAllValid $ \m2 ->
              withClient cenv $ \c1 ->
                withClient cenv $ \c2 -> do
                  let m = M.union m1 m2
                  setupClientContents c1 m1
                  setupClientContents c2 m2
                  syncSmosSyncClient c1
                  syncSmosSyncClient c2
                  assertClientContents c1 m
                  assertClientContents c2 m

assertClientContents :: SyncSettings -> Map (Path Rel File) ByteString -> IO ()
assertClientContents ss = assertContents (syncSetContentsDir ss)

assertContents :: Path Abs Dir -> Map (Path Rel File) ByteString -> IO ()
assertContents dir m = do
  m' <- readContents dir
  m' `shouldBe` m

readContents :: Path Abs Dir -> IO (Map (Path Rel File) ByteString)
readContents dir = do
  fs <- snd <$> listDirRecurRel dir
  fmap M.fromList $ forM fs $ \f -> (,) f <$> SB.readFile (fromAbsFile $ dir </> f)

setupClientContents :: SyncSettings -> Map (Path Rel File) ByteString -> IO ()
setupClientContents ss = setupContents (syncSetContentsDir ss)

setupContents :: Path Abs Dir -> Map (Path Rel File) ByteString -> IO ()
setupContents dir m = do
  let parents = S.map (parent . (dir </>)) (M.keysSet m)
  forM_ parents ensureDir
  forM_ (M.toList m) $ \(k, v) -> setupFile dir k v

setupFile :: Path Abs Dir -> Path Rel File -> ByteString -> IO ()
setupFile dir file contents = do
  let p = dir </> file
  SB.writeFile (fromAbsFile p) contents

withClient :: ClientEnv -> (SyncSettings -> IO ()) -> IO ()
withClient cenv func =
  withSystemTempDir "smos-sync-client-test-contents" $ \tmpDir1 ->
    withSystemTempDir "smos-sync-client-test-meta" $ \tmpDir2 -> do
      m <- resolveFile tmpDir2 "metadata.json"
      let ss =
            SyncSettings
              { syncSetServerUrl = baseUrl cenv
              , syncSetContentsDir = tmpDir1
              , syncSetMetadataFile = m
              }
      func ss

-- Remove this after upgrading to path-0.6.0
listDirRecurRel :: Path Abs Dir -> IO ([Path Rel Dir], [Path Rel File])
listDirRecurRel d = do
  (ds, fs) <- listDirRecur d
  pure (mapMaybe (stripProperPrefix d) ds, mapMaybe (stripProperPrefix d) fs)
