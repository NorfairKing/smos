module Smos.LaunchSpec where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Exception
import qualified Data.ByteString as SB
import Data.GenValidity.Path ()
import qualified Graphics.Vty as Vty (Config (..), defaultConfig, mkVty)
import Graphics.Vty.Output.TerminfoBased (setWindowSize)
import Path
import Path.IO
import Smos
import Smos.Data
import Smos.Data.Gen ()
import Smos.Default
import System.Environment
import System.Posix.IO
import System.Posix.Terminal
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
  (masterFd, slaveFd) <- openPseudoTerminal
  let vtyBuilder = do
        vty <- Vty.mkVty $ Vty.defaultConfig {Vty.inputFd = Just slaveFd, Vty.outputFd = Just slaveFd}
        setWindowSize slaveFd (80, 24)
        pure vty
  let runSmos = startSmosWithVtyBuilderOn vtyBuilder startupFile config
  withAsync runSmos $ \smosAsync -> do
    threadDelay $ 250 * 1000 -- Wait a bit to be sure that smos did the initialisation
    mErrOrDone <- poll smosAsync
    case mErrOrDone of
      Just (Left err) -> expectationFailure $ displayException err
      Just (Right ()) -> expectationFailure "Smos exited."
      Nothing -> cancel smosAsync
  masterHandle <- fdToHandle masterFd
  SB.hGetNonBlocking masterHandle 4096 >>= SB.putStr
