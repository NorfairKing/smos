{-# LANGUAGE FlexibleContexts #-}

module Smos.Sync.Client.TestUtils where

import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S

import Control.Monad

import Path
import Path.IO
import Servant.Client

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Validity

import Smos.Sync.Client.Contents
import Smos.Sync.Client.ContentsMap (ContentsMap(..))
import Smos.Sync.Client.OptParse
import Smos.Sync.Client.OptParse.Types
import Smos.Sync.Client.Sync.Gen ()

withTestDir :: SpecWith (Path Abs Dir) -> Spec
withTestDir = modifyMaxShrinks (const 0) . around (withSystemTempDir "smos-sync-client-save-test")

disjunctMap :: (Show k, Ord k, GenValid k, GenValid v) => Map k v -> Gen (Map k v)
disjunctMap m = genValid `suchThat` (\m' -> M.null $ M.intersection m m')

changedMap :: (Ord k, GenValid k, Eq v, GenValid v) => Map k v -> Gen (Map k v)
changedMap = traverse (\v -> genValid `suchThat` (/= v))

assertClientContents :: SyncSettings -> ContentsMap -> IO ()
assertClientContents ss = assertContents (syncSetContentsDir ss)

assertContents :: Path Abs Dir -> ContentsMap -> IO ()
assertContents dir m = do
  m' <- readContents dir
  m' `shouldBe` m

readContents :: Path Abs Dir -> IO ContentsMap
readContents dir = do
  fs <- snd <$> listDirRecurRel dir
  fmap (ContentsMap . M.fromList) $ forM fs $ \f -> (,) f <$> SB.readFile (fromAbsFile $ dir </> f)

setupClientContents :: SyncSettings -> ContentsMap -> IO ()
setupClientContents ss = setupContents (syncSetContentsDir ss)

setupContents :: Path Abs Dir -> ContentsMap -> IO ()
setupContents dir (ContentsMap m) = do
  resetDir dir
  let parents = S.map (parent . (dir </>)) (M.keysSet m)
  forM_ parents ensureDir
  forM_ (M.toList m) $ uncurry $ setupFile dir

setupFile :: Path Abs Dir -> Path Rel File -> ByteString -> IO ()
setupFile dir file contents = do
  let p = dir </> file
  SB.writeFile (fromAbsFile p) contents

resetDir :: Path Abs Dir -> IO ()
resetDir dir = do
  removeDirRecur dir
  ensureDir dir

forAllHidden :: (Testable prop) => (Path Rel File -> prop) -> Property
forAllHidden = forAllShrink (genProbablyHidden `suchThat` isHidden) (filter isHidden . shrinkValid)
  where
    genProbablyHidden = do
      hiddenThing <- genValid
      case parseRelFile $ "." ++ fromRelFile hiddenThing of
        Nothing -> genProbablyHidden
        Just f -> do
          base <- genValid
          pure $ base </> f

withHiddenFilesClient :: ClientEnv -> (SyncSettings -> IO a) -> IO a
withHiddenFilesClient cenv func =
  withClient cenv $ \ss -> func ss {syncSetIgnoreFiles = IgnoreHiddenFiles}

withClient :: ClientEnv -> (SyncSettings -> IO a) -> IO a
withClient cenv func =
  withSystemTempDir "smos-sync-client-test-contents" $ \tmpDir1 ->
    withSystemTempDir "smos-sync-client-test-meta" $ \tmpDir2 -> do
      m <- resolveFile tmpDir2 "metadata.json"
      let ss =
            SyncSettings
              { syncSetServerUrl = baseUrl cenv
              , syncSetContentsDir = tmpDir1
              , syncSetMetadataFile = m
              , syncSetIgnoreFiles = IgnoreNothing
              }
      func ss
