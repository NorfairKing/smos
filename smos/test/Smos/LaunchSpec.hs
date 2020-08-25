module Smos.LaunchSpec where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Exception
import Data.GenValidity.Path ()
import qualified Graphics.Vty as Vty (Config (..), defaultConfig, mkVty)
import Path
import Path.IO
import Smos
import Smos.Data
import Smos.Data.Gen ()
import Smos.Default
import System.Posix.Terminal
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Validity

spec :: Spec
spec = modifyMaxSuccess (`div` 50) $ do
  specify "Launching smos with a nonexistent workflow dir on a nonexistent file works fine" $ forAllValid $ \rd ->
    forAllValid $ \rf ->
      withSystemTempDir "smos-test" $ \td -> do
        let wd = td </> rd
        let file = td </> rf
        startupSpec wd file
  specify "Launching smos with a nonexistent workflow dir on an existent file works fine" $ forAllValid $ \rd ->
    forAllValid $ \rf ->
      forAllValid $ \sf ->
        withSystemTempDir "smos-test" $ \td -> do
          let wd = td </> rd
          let file = td </> rf
          writeSmosFile file sf
          startupSpec wd file
  specify "Launching smos with an existent workflow dir on a nonexistent file works fine" $ forAllValid $ \rd ->
    forAllValid $ \rf ->
      withSystemTempDir "smos-test" $ \td -> do
        let wd = td </> rd
        ensureDir wd
        let file = td </> rf
        startupSpec wd file
  specify "Launching smos with an existent workflow dir on an existent file works fine" $ forAllValid $ \rd ->
    forAllValid $ \rf ->
      forAllValid $ \sf ->
        withSystemTempDir "smos-test" $ \td -> do
          let wd = td </> rd
          ensureDir wd
          let file = td </> rf
          writeSmosFile file sf
          startupSpec wd file

startupSpec :: Path Abs Dir -> Path Abs File -> IO ()
startupSpec workflowDir startupFile = do
  let config =
        defaultConfig
          { configReportConfig =
              defaultReportConfig
                { smosReportConfigDirectoryConfig =
                    defaultDirectoryConfig
                      { directoryConfigWorkflowFileSpec = DirAbsolute workflowDir
                      }
                }
          }
  (_, slaveFd) <- openPseudoTerminal
  let vtyBuilder = Vty.mkVty $ Vty.defaultConfig {Vty.inputFd = Just slaveFd, Vty.outputFd = Just slaveFd}
  let runSmos = startSmosWithVtyBuilderOn vtyBuilder startupFile config
  withAsync runSmos $ \smosAsync -> do
    threadDelay $ 250 * 1000 -- Wait a bit to be sure that smos did the initialisation
    mErrOrDone <- poll smosAsync
    case mErrOrDone of
      Just (Left err) -> expectationFailure $ displayException err
      Just (Right ()) -> expectationFailure "Smos exited."
      Nothing -> cancel smosAsync
