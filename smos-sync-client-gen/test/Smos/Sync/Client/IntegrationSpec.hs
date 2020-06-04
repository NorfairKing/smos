module Smos.Sync.Client.IntegrationSpec
  ( spec,
  )
where

import qualified Data.Text as T
import Path
import Path.IO
import Servant.Client
import Smos.API
import Smos.Server.TestUtils
import Smos.Sync.Client
import System.Environment
import Test.Hspec
import Test.Validity

spec :: Spec
spec =
  serverSpec
    $ describe "smos-sync-client"
    $ do
      it "just works (tm) with manual login" $ \cenv ->
        forAllValid $ \un ->
          forAllValid $ \pw ->
            withSystemTempDir "smos-sync-client-test-contents" $ \contentsDir ->
              withSystemTempDir "smos-sync-client-test-meta" $ \tmpDir -> do
                let t = test cenv tmpDir
                t ["register", "--username", usernameString un, "--password", T.unpack $ unsafeShowPassword pw]
                t ["login", "--username", usernameString un, "--password", T.unpack $ unsafeShowPassword pw]
                up <- resolveFile tmpDir "uuid.jon"
                mp <- resolveFile tmpDir "metadata.db"
                t
                  [ "sync",
                    "--contents-dir",
                    fromAbsDir contentsDir,
                    "--metadata-db",
                    fromAbsFile mp,
                    "--uuid-file",
                    fromAbsFile up
                  ]
      it "just works (tm) without manual login" $ \cenv ->
        forAllValid $ \un ->
          forAllValid $ \pw ->
            withSystemTempDir "smos-sync-client-test-contents" $ \contentsDir ->
              withSystemTempDir "smos-sync-client-test-meta" $ \tmpDir -> do
                let t = test cenv tmpDir
                t ["register", "--username", usernameString un, "--password", T.unpack $ unsafeShowPassword pw]
                up <- resolveFile tmpDir "uuid.jon"
                mp <- resolveFile tmpDir "metadata.db"
                t
                  [ "--username",
                    usernameString un,
                    "--password",
                    T.unpack $ unsafeShowPassword pw,
                    "sync",
                    "--contents-dir",
                    fromAbsDir contentsDir,
                    "--metadata-db",
                    fromAbsFile mp,
                    "--uuid-file",
                    fromAbsFile up
                  ]
      it "just works (tm) when the contents dir doesn't exist yet" $ \cenv ->
        forAllValid $ \un ->
          forAllValid $ \pw ->
            withSystemTempDir "smos-sync-client-test-contents" $ \contentsDir ->
              withSystemTempDir "smos-sync-client-test-meta" $ \tmpDir -> do
                let t = test cenv tmpDir
                t ["register", "--username", usernameString un, "--password", T.unpack $ unsafeShowPassword pw]
                cd <- resolveDir contentsDir "subdir/for/contents"
                up <- resolveFile tmpDir "uuid.jon"
                mp <- resolveFile tmpDir "metadata.db"
                t
                  [ "--username",
                    usernameString un,
                    "--password",
                    T.unpack $ unsafeShowPassword pw,
                    "sync",
                    "--contents-dir",
                    fromAbsDir cd,
                    "--metadata-db",
                    fromAbsFile mp,
                    "--uuid-file",
                    fromAbsFile up
                  ]
      it "just works (tm) when the directory for the metadata doesn't exist yet" $ \cenv ->
        forAllValid $ \un ->
          forAllValid $ \pw ->
            withSystemTempDir "smos-sync-client-test-contents" $ \contentsDir ->
              withSystemTempDir "smos-sync-client-test-meta" $ \tmpDir -> do
                let t = test cenv tmpDir
                t ["register", "--username", usernameString un, "--password", T.unpack $ unsafeShowPassword pw]
                tmpSubDir <- resolveDir tmpDir "subdir/for/metadata"
                up <- resolveFile tmpDir "uuid.jon"
                mp <- resolveFile tmpSubDir "metadata.db"
                t
                  [ "--username",
                    usernameString un,
                    "--password",
                    T.unpack $ unsafeShowPassword pw,
                    "sync",
                    "--contents-dir",
                    fromAbsDir contentsDir,
                    "--metadata-db",
                    fromAbsFile mp,
                    "--uuid-file",
                    fromAbsFile up
                  ]
      it "just works (tm) when the directory for the uuid file doesn't exist yet" $ \cenv ->
        forAllValid $ \un ->
          forAllValid $ \pw ->
            withSystemTempDir "smos-sync-client-test-contents" $ \contentsDir ->
              withSystemTempDir "smos-sync-client-test-meta" $ \tmpDir -> do
                let t = test cenv tmpDir
                t ["register", "--username", usernameString un, "--password", T.unpack $ unsafeShowPassword pw]
                tmpSubDir <- resolveDir tmpDir "subdir/for/uuid"
                up <- resolveFile tmpSubDir "uuid.jon"
                mp <- resolveFile tmpDir "metadata.db"
                t
                  [ "--username",
                    usernameString un,
                    "--password",
                    T.unpack $ unsafeShowPassword pw,
                    "sync",
                    "--contents-dir",
                    fromAbsDir contentsDir,
                    "--metadata-db",
                    fromAbsFile mp,
                    "--uuid-file",
                    fromAbsFile up
                  ]

test :: ClientEnv -> Path Abs Dir -> [String] -> IO ()
test cenv tmpDir args = do
  sp <- resolveFile tmpDir "session.dat"
  let args' = args ++ ["--server-url", showBaseUrl $ baseUrl cenv, "--session-path", fromAbsFile sp]
  putStrLn $ unwords ["running", unwords $ map show $ "smos-sync-client" : args']
  withArgs args' smosSyncClient
