module Smos.LaunchSpec where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Exception
import Data.GenValidity.Path ()
import Path
import Path.IO
import Smos
import Smos.Data
import Smos.Data.Gen ()
import Smos.Default
import Smos.Instance
import System.Environment
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Validity

spec :: Spec
spec = modifyMaxSuccess (`div` 50) $ do
  describe "Launching smos with" $ do
    describe "a specified" $ do
      describe "nonexistent workflow dir" $ do
        specify "on a nonexistent file works fine" $ forAllValid $ \rd ->
          forAllValid $ \rf ->
            withSystemTempDir "smos-test" $ \td -> do
              let wd = td </> rd
              let file = td </> rf
              startupSpec (DirAbsolute wd) file
        specify "on an existent file works fine" $ forAllValid $ \rd ->
          forAllValid $ \rf ->
            forAllValid $ \sf ->
              withSystemTempDir "smos-test" $ \td -> do
                let wd = td </> rd
                let file = td </> rf
                writeSmosFile file sf
                startupSpec (DirAbsolute wd) file
      describe "existent workflow dir" $ do
        specify "on a nonexistent file works fine" $ forAllValid $ \rd ->
          forAllValid $ \rf ->
            withSystemTempDir "smos-test" $ \td -> do
              let wd = td </> rd
              ensureDir wd
              let file = td </> rf
              startupSpec (DirAbsolute wd) file
        specify "on an existent file works fine" $ forAllValid $ \rd ->
          forAllValid $ \rf ->
            forAllValid $ \sf ->
              withSystemTempDir "smos-test" $ \td -> do
                let wd = td </> rd
                ensureDir wd
                let file = td </> rf
                writeSmosFile file sf
                startupSpec (DirAbsolute wd) file
    describe "an unspecified" $ do
      describe "nonexistent workflow dir" $ do
        specify "on a nonexistent file works fine" $ forAllValid $ \rd ->
          forAllValid $ \homeRel ->
            forAllValid $ \rf ->
              withSystemTempDir "smos-test" $ \td -> do
                let home = td </> homeRel
                let file = td </> rf
                setEnv "HOME" $ fromAbsDir home
                startupSpec (DirInHome rd) file
        specify "on an existent file works fine" $ forAllValid $ \rd ->
          forAllValid $ \homeRel ->
            forAllValid $ \rf ->
              forAllValid $ \sf ->
                withSystemTempDir "smos-test" $ \td -> do
                  let home = td </> homeRel
                  let file = td </> rf
                  writeSmosFile file sf
                  setEnv "HOME" $ fromAbsDir home
                  startupSpec (DirInHome rd) file
      describe "existent workflow dir" $ do
        specify "on a nonexistent file works fine" $ forAllValid $ \rd ->
          forAllValid $ \homeRel ->
            forAllValid $ \rf ->
              withSystemTempDir "smos-test" $ \td -> do
                let home = td </> homeRel
                ensureDir $ home </> rd
                let file = td </> rf
                setEnv "HOME" $ fromAbsDir home
                startupSpec (DirInHome rd) file
        specify "on an existent file works fine" $ forAllValid $ \rd ->
          forAllValid $ \homeRel ->
            forAllValid $ \rf ->
              forAllValid $ \sf ->
                withSystemTempDir "smos-test" $ \td -> do
                  let home = td </> homeRel
                  ensureDir $ home </> rd
                  let file = td </> rf
                  writeSmosFile file sf
                  setEnv "HOME" $ fromAbsDir home
                  startupSpec (DirInHome rd) file

startupSpec :: WorkflowDirSpec -> Path Abs File -> IO ()
startupSpec workflowDirSpec startupFile = do
  let config =
        defaultConfig
          { configReportConfig =
              defaultReportConfig
                { smosReportConfigDirectoryConfig =
                    defaultDirectoryConfig
                      { directoryConfigWorkflowFileSpec = workflowDirSpec
                      }
                }
          }
  withSmosInstance config startupFile $ \smosHandle -> do
    threadDelay $ 250 * 1000 -- Wait a bit to be sure that smos did the initialisation
    let smosAsync = smosInstanceHandleAsync smosHandle
    link smosAsync
    mErrOrDone <- poll smosAsync
    case mErrOrDone of
      Just (Left err) -> expectationFailure $ displayException err
      Just (Right ()) -> expectationFailure "Smos exited."
      Nothing -> cancel smosAsync
