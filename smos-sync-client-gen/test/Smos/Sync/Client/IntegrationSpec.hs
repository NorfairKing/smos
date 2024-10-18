module Smos.Sync.Client.IntegrationSpec
  ( spec,
  )
where

import Control.Exception
import Control.Monad
import qualified Data.Text as T
import Path
import Path.IO
import Servant.Client
import Smos.API
import Smos.Server.TestUtils
import Smos.Sync.Client
import System.Environment
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec =
  serverSpec $
    sequential $
      describe "smos-sync-client" $ do
        it "just works (tm) with manual login" $ \cenv ->
          forAllValid $ \un ->
            forAllValid $ \pw ->
              withSystemTempDir "smos-sync-client-test-contents" $ \contentsDir ->
                withSystemTempDir "smos-sync-client-test-meta" $ \tmpDir -> do
                  let t = test cenv tmpDir
                  t ["register", "--username", usernameString un, "--password", T.unpack $ unsafeShowPassword pw]
                  t ["login", "--username", usernameString un, "--password", T.unpack $ unsafeShowPassword pw]
                  t
                    [ "sync",
                      "--contents-dir",
                      fromAbsDir contentsDir
                    ]
        it "just works (tm) without manual login" $ \cenv ->
          forAllValid $ \un ->
            forAllValid $ \pw ->
              withSystemTempDir "smos-sync-client-test-contents" $ \contentsDir ->
                withSystemTempDir "smos-sync-client-test-meta" $ \tmpDir -> do
                  let t = test cenv tmpDir
                  t ["register", "--username", usernameString un, "--password", T.unpack $ unsafeShowPassword pw]
                  t
                    [ "--username",
                      usernameString un,
                      "--password",
                      T.unpack $ unsafeShowPassword pw,
                      "sync",
                      "--contents-dir",
                      fromAbsDir contentsDir
                    ]
        it "just works (tm) when the contents dir doesn't exist yet" $ \cenv ->
          forAllValid $ \un ->
            forAllValid $ \pw ->
              withSystemTempDir "smos-sync-client-test-contents" $ \contentsDir ->
                withSystemTempDir "smos-sync-client-test-meta" $ \tmpDir -> do
                  let t = test cenv tmpDir
                  t ["register", "--username", usernameString un, "--password", T.unpack $ unsafeShowPassword pw]
                  cd <- resolveDir contentsDir "subdir/for/contents"
                  t
                    [ "--username",
                      usernameString un,
                      "--password",
                      T.unpack $ unsafeShowPassword pw,
                      "sync",
                      "--contents-dir",
                      fromAbsDir cd
                    ]
        it "just works (tm) when the directory for the metadata doesn't exist yet" $ \cenv ->
          forAllValid $ \un ->
            forAllValid $ \pw ->
              withSystemTempDir "smos-sync-client-test-contents" $ \contentsDir ->
                withSystemTempDir "smos-sync-client-test-meta" $ \tmpDir -> do
                  let t = test cenv tmpDir
                  t ["register", "--username", usernameString un, "--password", T.unpack $ unsafeShowPassword pw]
                  tmpSubDir <- resolveDir tmpDir "subdir/for/metadata"
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
                      fromAbsFile mp
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
                  t
                    [ "--username",
                      usernameString un,
                      "--password",
                      T.unpack $ unsafeShowPassword pw,
                      "sync",
                      "--contents-dir",
                      fromAbsDir contentsDir,
                      "--uuid-file",
                      fromAbsFile up
                    ]

test :: ClientEnv -> Path Abs Dir -> [String] -> IO ()
test cenv tmpDir args = do
  let extraEnv = [("SMOS_SYNC_CLIENT_DATA_DIR", fromAbsDir tmpDir)]
  let args' =
        args
          ++ [ "--server-url",
               showBaseUrl $ baseUrl cenv,
               "--cache-dir",
               fromAbsDir tmpDir,
               "--log-level",
               -- You can turn this into Debug during debugging if necessary
               "Error"
             ]
  withExtraEnv extraEnv $ withArgs args' smosSyncClient

withExtraEnv :: [(String, String)] -> IO a -> IO a
withExtraEnv extraEnv func = do
  envBefore <- forM (map fst extraEnv) $ \k -> (,) k <$> lookupEnv k
  let resetEnv = forM envBefore $ \(k, mv) -> case mv of
        Nothing -> unsetEnv k
        Just v -> setEnv k v
  let setExtraEnv = mapM (uncurry setEnv) extraEnv
  bracket_ setExtraEnv resetEnv func
