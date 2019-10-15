module Smos.Sync.Client.IntegrationSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck
import Test.Validity

import qualified Data.Text as T

import Path
import Path.IO

import System.Environment

import Servant.Client

import Smos.API

import Smos.Server.TestUtils

import Smos.Sync.Client
import Smos.Sync.Client.TestUtils

spec :: Spec
spec =
  serverSpec $
  describe "smos-sync-client" $ do
    it "just works (tm) with manual login" $ \cenv ->
      forAllValid $ \un ->
        forAllValid $ \pw ->
          withSystemTempDir "smos-sync-client-test-contents" $ \contentsDir ->
            withSystemTempDir "smos-sync-client-test-meta" $ \tmpDir -> do
              let t = test cenv tmpDir
              t ["register", "--username", usernameString un, "--password", passwordString pw]
              t ["login", "--username", usernameString un, "--password", passwordString pw]
              up <- resolveFile tmpDir "uuid.jon"
              mp <- resolveFile tmpDir "metadata.db"
              t
                [ "sync"
                , "--contents-dir"
                , fromAbsDir contentsDir
                , "--metadata-db"
                , fromAbsFile mp
                , "--uuid-file"
                , fromAbsFile up
                ]
    it "just works (tm) without manual login" $ \cenv ->
      forAllValid $ \un ->
        forAllValid $ \pw ->
          withSystemTempDir "smos-sync-client-test-contents" $ \contentsDir -> do
            withSystemTempDir "smos-sync-client-test-meta" $ \tmpDir -> do
              let t = test cenv tmpDir
              t ["register", "--username", usernameString un, "--password", passwordString pw]
              up <- resolveFile tmpDir "uuid.jon"
              mp <- resolveFile tmpDir "metadata.db"
              t
                [ "--username"
                , usernameString un
                , "--password"
                , passwordString pw
                , "sync"
                , "--contents-dir"
                , fromAbsDir contentsDir
                , "--metadata-db"
                , fromAbsFile mp
                , "--uuid-file"
                , fromAbsFile up
                ]

test :: ClientEnv -> Path Abs Dir -> [String] -> IO ()
test cenv tmpDir args = do
  sp <- resolveFile tmpDir "session.dat"
  let args' = args ++ ["--server-url", showBaseUrl $ baseUrl cenv, "--session-path", fromAbsFile sp]
  putStrLn $ unwords $ ["running", unwords $ map show $ "smos-sync-client" : args']
  withArgs args' smosSyncClient
