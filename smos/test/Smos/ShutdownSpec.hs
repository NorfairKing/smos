module Smos.ShutdownSpec where

import Control.Concurrent.Async (AsyncCancelled (..))
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
import System.FileLock
import Test.Syd
import Test.Syd.Validity
import UnliftIO
import UnliftIO.Concurrent

spec :: Spec
spec = modifyMaxSuccess (`div` 50) $
  sequential $ do
    describe "Shutting down a smos file" $ do
      -- This first test is just sanity checking
      it "cannot lock the same file two times at the same time" $ do
        forAllValid $ \relFile ->
          withSystemTempDir "smos-shutdown-test" $ \tdir -> do
            let startupFile = tdir </> relFile
            res1 <- tryLockFile (fromAbsFile startupFile) Exclusive
            case res1 of
              Nothing -> expectationFailure "Locking the first time should not fail."
              Just _ -> pure ()
            res2 <- tryLockFile (fromAbsFile startupFile) Exclusive
            case res2 of
              Nothing -> pure () -- Success
              Just _ -> expectationFailure "Locking should have failed the second time."
      it "releases the file lock when cancelled" $ do
        forAllValid $ \sf ->
          forAllValid $ \relFile ->
            withSystemTempDir "smos-shutdown-test" $ \tdir -> do
              let startupFile = tdir </> relFile
              writeSmosFile startupFile sf
              let config =
                    defaultConfig
                      { configReportConfig =
                          defaultReportConfig
                            { smosReportConfigDirectoryConfig =
                                defaultDirectoryConfig
                                  { directoryConfigWorkflowFileSpec = AbsoluteWorkflow tdir
                                  }
                            }
                      }
              withSmosInstance config (Just $ StartingFile startupFile) $ \smos1 -> do
                threadDelay $ 50 * 1000 -- Wait a bit to be sure that smos definitely tried to take the lock
                let smos1Async = terminalHandleAsync smos1
                mErrOrDone1 <- poll smos1Async
                case mErrOrDone1 of
                  Just (Left err) -> expectationFailure $ displayException err
                  Just (Right ()) -> expectationFailure "Smos 1 exited."
                  Nothing -> pure ()
                cancel smos1Async -- Use waitCatch instead of this and use a timeout
                res1 <- poll smos1Async
                case res1 of
                  Nothing -> expectationFailure "Smos should have exited by now."
                  Just (Left e) -> case fromException e of
                    Nothing -> expectationFailure "Should not have failed differently."
                    Just AsyncCancelled -> pure ()
                  Just (Right ()) -> expectationFailure "Should have been canceled, not exited normally."
                withSmosInstance config (Just $ StartingFile startupFile) $ \smos2 -> do
                  threadDelay $ 50 * 1000 -- Wait a bit to be sure that smos definitely tried to take the lock and exited
                  let smos2Async = terminalHandleAsync smos2
                  res2 <- poll smos2Async
                  case res2 of
                    Nothing -> pure ()
                    Just (Left e) -> expectationFailure $ "Smos two should not have failed, but got this exception: " <> displayException e
                    Just (Right ()) -> expectationFailure "Should not have errored."
