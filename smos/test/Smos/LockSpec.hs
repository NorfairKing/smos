module Smos.LockSpec where

import Control.Exception
import Data.GenValidity.Path ()
import Path
import Path.IO
import Smos
import Smos.Data
import Smos.Data.Gen ()
import Smos.Default
import Smos.Instance
import Smos.Terminal
import Smos.Types
import System.Exit
import Test.Syd
import Test.Syd.Validity
import UnliftIO
import UnliftIO.Concurrent

spec :: Spec
spec = modifyMaxSuccess (`div` 50) $
  sequential $ do
    describe "Launching smos twice with the same startup file" $ do
      describe "on a nonexistent file" $ do
        describe "in the workflow dir" $ do
          it "fails to start if the file is already locked." $
            forAllValid $ \rf -> withSystemTempDir "smos-lock-test" $ \workflowDir -> do
              let startupFile = workflowDir </> rf
              lockSpec workflowDir startupFile
        describe "outside the workflow dir" $ do
          it "fails to start if the file is already locked." $
            forAllValid $ \rf ->
              withSystemTempDir "smos-lock-test" $ \tdir -> do
                let startupFile = tdir </> rf
                workflowDir <- resolveDir tdir "workflow"
                lockSpec workflowDir startupFile
      describe "on an existent file" $ do
        describe "in the workflow dir" $ do
          it "fails to start if the file is already locked." $
            forAllValid $ \rf ->
              forAllValid $ \sf ->
                withSystemTempDir "smos-lock-test" $ \workflowDir -> do
                  let startupFile = workflowDir </> rf
                  writeSmosFile startupFile sf
                  lockSpec workflowDir startupFile
        describe "outside the workflow dir" $ do
          it "fails to start if the file is already locked." $
            forAllValid $ \rf ->
              forAllValid $ \sf ->
                withSystemTempDir "smos-lock-test" $ \tdir -> do
                  let startupFile = tdir </> rf
                  writeSmosFile startupFile sf
                  workflowDir <- resolveDir tdir "workflow"
                  lockSpec workflowDir startupFile

lockSpec :: Path Abs Dir -> Path Abs File -> IO ()
lockSpec workflowDir startupFile = do
  let config =
        defaultConfig
          { configReportConfig =
              defaultReportConfig
                { smosReportConfigDirectoryConfig =
                    defaultDirectoryConfig
                      { directoryConfigWorkflowFileSpec = AbsoluteWorkflow workflowDir
                      }
                }
          }
  withSmosInstance config (Just $ StartingFile startupFile) $ \smos1 -> do
    threadDelay $ 50 * 1000 -- Wait a bit to be sure that smos 1 did the initialisation
    let smos1Async = terminalHandleAsync smos1
    link smos1Async
    mErrOrDone1 <- poll smos1Async
    case mErrOrDone1 of
      Just (Left err) -> expectationFailure $ displayException err
      Just (Right ()) -> expectationFailure "Smos 1 exited."
      Nothing -> pure ()
    withSmosInstance config (Just $ StartingFile startupFile) $ \smos2 -> do
      threadDelay $ 50 * 1000 -- Wait a bit to be sure that smos 2 did the initialisation
      let smos2Async = terminalHandleAsync smos2
      mErrOrDone2 <- poll smos2Async
      case mErrOrDone2 of
        Just (Left e) -> case fromException e of
          Just (ExitFailure 1) -> pure ()
          _ -> expectationFailure $ displayException e
        Just (Right ()) -> expectationFailure "Smos 2 exited."
        Nothing -> expectationFailure "Smos 2 should have exited by now."
