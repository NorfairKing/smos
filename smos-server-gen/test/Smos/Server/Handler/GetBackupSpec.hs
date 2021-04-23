{-# LANGUAGE QuasiQuotes #-}

module Smos.Server.Handler.GetBackupSpec
  ( spec,
  )
where

import Codec.Archive.Zip as Zip
import qualified Data.ByteString as SB
import qualified Data.DirForest as DF
import qualified Data.Map as M
import qualified Data.Text as T
import Path
import Path.IO
import Servant.Types.SourceT
import Smos.Client
import Smos.Data
import Smos.Data.Gen ()
import Smos.Report.InterestingStore
import Smos.Server.InterestingStore
import Smos.Server.TestUtils
import System.IO
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec =
  describe "GetBackup" $ do
    serverSpec $ do
      it "can go through the intergration test for any valid store" $ \cenv ->
        forAllValid $ \store -> backupIntegrationTestForInterestingStore cenv store
      it "has no problem with a file that have weird filenames" $ \cenv -> do
        let store = emptyInterestingStore {archiveFiles = DF.singletonFile [relfile|example_2021-03-24_10:46:51.smos|] emptySmosFile}
        backupIntegrationTestForInterestingStore cenv store
      modifyMaxSize (* 100) $
        -- We need to have two of these, not one
        -- because the first is always done with a small size
        modifyMaxSuccess (const 2) $ do
          it "has no problem with a large file" $ \cenv ->
            forAllValid $ \sf -> do
              let store = emptyInterestingStore {archiveFiles = DF.singletonFile [relfile|dummp.smos|] sf}
              backupIntegrationTestForInterestingStore cenv store
          it "has no problem with a large store" $ \cenv ->
            forAllValid $ \store -> do
              backupIntegrationTestForInterestingStore cenv store

backupIntegrationTestForInterestingStore :: ClientEnv -> InterestingStore -> IO ()
backupIntegrationTestForInterestingStore cenv store =
  withNewUser cenv $ \t -> withSystemTempDir "smos-server-test-get-backup" $ \tdir -> do
    archiveFile <- resolveFile tdir "archive.zip"
    expectedFiles <- testClient cenv $ do
      setupInterestingStore t store
      clientGetListSmosFiles t
    withFile (fromAbsFile archiveFile) WriteMode $ \handle ->
      testClient cenv $ do
        backupUuid <- clientPostBackup t
        archiveSource <- clientGetBackup t backupUuid
        liftIO $ foreach expectationFailure (liftIO . SB.hPut handle) archiveSource
    unpackDir <- resolveDir tdir "unpack-dir"
    Zip.withArchive (fromAbsFile archiveFile) $ do
      Zip.unpackInto (fromAbsDir unpackDir)
    actualFiles <- DF.read unpackDir $ \p ->
      if fileExtension p == Just ".smos"
        then do
          mErrOrSmosFile <- readSmosFile p
          case mErrOrSmosFile of
            Nothing -> expectationFailure $ unlines ["File unexpectedly did not exist:", fromAbsFile p]
            Just (Left _) -> pure Nothing
            Just (Right sf) -> pure $ Just sf
        else pure Nothing -- Ignore non-smos files because 'clientGetListSmosFiles' doesn't actually contain the non-smos files.
        -- We have to turn the files to UTF8 so that the replacement characters are in place on both sides of the equation.
    M.mapKeys (T.pack . fromRelFile) (M.mapMaybe id (DF.toFileMap actualFiles)) `shouldBe` M.mapKeys (T.pack . fromRelFile) (DF.toFileMap expectedFiles)
