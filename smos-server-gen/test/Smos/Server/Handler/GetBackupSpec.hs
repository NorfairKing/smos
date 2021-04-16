module Smos.Server.Handler.GetBackupSpec
  ( spec,
  )
where

import Codec.Archive.Zip as Zip
import qualified Data.ByteString as SB
import qualified Data.DirForest as DF
import Path
import Path.IO
import Servant.Types.SourceT
import Smos.Client
import Smos.Data
import Smos.Data.Gen ()
import Smos.Server.InterestingStore
import Smos.Server.TestUtils
import System.IO
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec =
  describe "GetBackup" $ do
    serverSpec $
      it "correctly lists two backups after making two backups" $ \cenv ->
        forAllValid $ \store ->
          withNewUser cenv $ \t -> withSystemTempDir "smos-server-test-get-backup" $ \tdir -> do
            archiveFile <- resolveFile tdir "archive.zip"
            expectedFiles <- testClient cenv $ clientGetListSmosFiles t
            withFile (fromAbsFile archiveFile) WriteMode $ \handle ->
              testClient cenv $ do
                setupInterestingStore t store
                backupUuid <- clientPostBackup t
                archiveSource <- clientGetBackup t backupUuid
                liftIO $ foreach expectationFailure (liftIO . SB.hPut handle) archiveSource
            unpackDir <- resolveDir tdir "unpack-dir"
            Zip.withArchive (fromAbsFile archiveFile) $ do
              Zip.unpackInto (fromAbsDir unpackDir)
            actualFiles <- DF.read unpackDir $ \p -> do
              mErrOrSmosFile <- readSmosFile p
              case mErrOrSmosFile of
                Just (Right sf) -> pure sf
                _ ->
                  expectationFailure $
                    unlines
                      [ "Something went wrong with file",
                        fromAbsFile p
                      ]
            actualFiles `shouldBe` expectedFiles
