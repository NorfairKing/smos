{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Sync.Client.TestUtils where

import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.Map as M
import Data.Map (Map)
import Data.Pool
import qualified Data.Set as S

import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger

import Lens.Micro
import Path
import Path.IO
import Servant.Client

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Validity

import Database.Persist.Sqlite as DB

import Smos.API

import Smos.Sync.Client.Command.Sync
import Smos.Sync.Client.Contents
import Smos.Sync.Client.ContentsMap (ContentsMap(..))
import Smos.Sync.Client.DB
import Smos.Sync.Client.OptParse
import Smos.Sync.Client.OptParse.Types
import Smos.Sync.Client.Sync.Gen ()

clientDBSpec :: SpecWith (Pool SqlBackend) -> Spec
clientDBSpec = modifyMaxShrinks (const 0) . modifyMaxSuccess (`div` 10) . around withClientDB

withClientDB :: (Pool SqlBackend -> IO a) -> IO a
withClientDB func =
  runNoLoggingT $
  DB.withSqlitePoolInfo (mkSqliteConnectionInfo ":memory:" & fkEnabled .~ False) 1 $ \pool -> do
    DB.runSqlPool (void $ DB.runMigrationSilent migrateAll) pool
    liftIO $ func pool

withTestDir :: SpecWith (Path Abs Dir) -> Spec
withTestDir = modifyMaxShrinks (const 0) . around (withSystemTempDir "smos-sync-client-save-test")

disjunctMap :: (Show k, Ord k, GenValid k, GenValid v) => Map k v -> Gen (Map k v)
disjunctMap m = genValid `suchThat` (\m' -> M.null $ M.intersection m m')

changedMap :: (Ord k, GenValid k, Eq v, GenValid v) => Map k v -> Gen (Map k v)
changedMap = traverse (\v -> genValid `suchThat` (/= v))

readClientContents :: SyncClientSettings -> IO ContentsMap
readClientContents (SyncClientSettings ss _) = readContents (syncSetContentsDir ss)

assertClientContents :: SyncClientSettings -> ContentsMap -> IO ()
assertClientContents (SyncClientSettings ss _) = assertContents (syncSetContentsDir ss)

assertContents :: Path Abs Dir -> ContentsMap -> IO ()
assertContents dir m = do
  m' <- readContents dir
  m' `shouldBe` m

readContents :: Path Abs Dir -> IO ContentsMap
readContents dir = do
  fs <- snd <$> listDirRecurRel dir
  fmap (ContentsMap . M.fromList) $ forM fs $ \f -> (,) f <$> SB.readFile (fromAbsFile $ dir </> f)

setupClientContents :: SyncClientSettings -> ContentsMap -> IO ()
setupClientContents (SyncClientSettings ss _) = setupContents (syncSetContentsDir ss)

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

data SyncClientSettings =
  SyncClientSettings SyncSettings Settings

withSyncClient :: ClientEnv -> Register -> (SyncClientSettings -> IO a) -> IO a
withSyncClient cenv reg func =
  withSystemTempDir "smos-sync-client-test-contents" $ \tmpDir1 ->
    withSystemTempDir "smos-sync-client-test-meta" $ \tmpDir2 -> do
      mp <- resolveFile tmpDir2 "metadata.sqlite3"
      up <- resolveFile tmpDir2 "uuid.json"
      sp <- resolveFile tmpDir2 "session.dat"
      let ss =
            SyncSettings
              { syncSetContentsDir = tmpDir1
              , syncSetMetadataDB = mp
              , syncSetUUIDFile = up
              , syncSetIgnoreFiles = IgnoreNothing
              }
      let s =
            Settings
              { setServerUrl = baseUrl cenv
              , setLogLevel = LevelWarn
              , setUsername = Just $ registerUsername reg
              , setPassword = Just $ registerPassword reg
              , setSessionPath = sp
              }
      let scs = SyncClientSettings ss s
      func scs

withHiddenFilesClient :: ClientEnv -> Register -> (SyncClientSettings -> IO a) -> IO a
withHiddenFilesClient cenv reg func =
  withSyncClient cenv reg $ \(SyncClientSettings ss s) ->
    let scs' = SyncClientSettings (ss {syncSetIgnoreFiles = IgnoreHiddenFiles}) s
     in func scs'

testSyncSmosClient :: SyncClientSettings -> IO ()
testSyncSmosClient (SyncClientSettings ss s) = syncSmosSyncClient s ss

fullySyncTwoClients :: SyncClientSettings -> SyncClientSettings -> IO ()
fullySyncTwoClients c1 c2 = fullySyncClients [c1, c2]

fullySyncClients :: [SyncClientSettings] -> IO ()
fullySyncClients cs = do
  let twice f = f >> f
  twice $ mapM_ testSyncSmosClient cs

fullySyncTwoClientsConcurrently :: SyncClientSettings -> SyncClientSettings -> IO ()
fullySyncTwoClientsConcurrently c1 c2 = fullySyncClientsConcurrently [c1, c2]

fullySyncClientsConcurrently :: [SyncClientSettings] -> IO ()
fullySyncClientsConcurrently cs = do
  let twice f = f >> f
  twice $ mapConcurrently_ testSyncSmosClient cs
