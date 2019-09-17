module Smos.Sync.Client.TestUtils where

import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import qualified Data.Set as S

import Control.Monad

import Path
import Path.IO

import Servant.Client

import Test.Hspec
import Test.QuickCheck
import Test.Validity

import Smos.Sync.Client.OptParse
import Smos.Sync.Client.Sync.Gen ()

withTestDir :: SpecWith (Path Abs Dir) -> Spec
withTestDir = around $ withSystemTempDir "smos-sync-client-save-test"

disjunctMap :: (Ord k, GenValid k, GenValid v) => Map k v -> Gen (Map k v)
disjunctMap m = genValid `suchThat` (\m' -> M.null $ M.intersection m m')

changedMap :: (Ord k, GenValid k, GenValid v) => Map k v -> Gen (Map k v)
changedMap = traverse (const genValid)

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
