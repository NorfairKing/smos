module Smos.ShutdownSpec where

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
import System.FileLock
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Validity

spec :: Spec
spec = modifyMaxSuccess (`div` 50) $ do
  describe "Shutting down a smos file" $ do
    -- This first test is just sanity checking
    it "cannot lock two files at the same time" $ do
      forAllValid $ \relFile ->
        withSystemTempDir "smos-shutdown-test" $ \tdir -> do
          let startupFile = tdir </> relFile
          res1 <- tryLockFile (fromAbsFile startupFile) Exclusive
          case res1 of
            Nothing -> expectationFailure "This shouldn't fail."
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
                                { directoryConfigWorkflowFileSpec = DirAbsolute tdir
                                }
                          }
                    }
            withSmosInstance config startupFile $ \smos1 -> do
              threadDelay $ 250 * 1000 -- Wait a bit to be sure that smos definitely tried to take the lock
              let smos1Async = smosInstanceHandleAsync smos1
              mErrOrDone1 <- poll smos1Async
              case mErrOrDone1 of
                Just (Left err) -> expectationFailure $ displayException err
                Just (Right ()) -> expectationFailure "Smos 1 exited."
                Nothing -> pure ()
              cancel smos1Async
              threadDelay $ 250 * 1000 -- Wait a bit to be sure that smos 1 is definitely done
              res1 <- poll smos1Async
              case res1 of
                Nothing -> expectationFailure "Smos should have exited by now."
                Just (Left e) -> case fromException e of
                  Nothing -> expectationFailure "Should not have failed differently."
                  Just AsyncCancelled -> pure ()
                Just (Right ()) -> expectationFailure "Should have been canceled, not exited normally."
              withSmosInstance config startupFile $ \smos2 -> do
                threadDelay $ 250 * 1000 -- Wait a bit to be sure that smos definitely tried to take the lock
                let smos2Async = smosInstanceHandleAsync smos2
                mErrOrDone2 <- poll smos2Async
                case mErrOrDone2 of
                  Just (Left err) -> expectationFailure $ displayException err
                  Just (Right ()) -> expectationFailure "Smos 2 exited."
                  Nothing -> pure ()
                threadDelay $ 250 * 1000 -- Wait a bit to be sure that smos 2 has definitely exited
                res2 <- poll smos2Async
                case res2 of
                  Nothing -> pure () -- expectationFailure "Smos two should not have exited normally."
                  Just (Left e) -> expectationFailure $ "Smos two should not have failed, but got this exception: " <> displayException e
                  Just (Right ()) -> expectationFailure "Should not have errored."
