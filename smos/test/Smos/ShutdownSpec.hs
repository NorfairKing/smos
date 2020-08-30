module Smos.ShutdownSpec where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Exception
import Data.GenValidity.Path ()
import qualified Graphics.Vty as Vty (Config (..), defaultConfig, mkVty)
import Graphics.Vty.Output.TerminfoBased (setWindowSize)
import Path
import Path.IO
import Smos
import Smos.Data
import Smos.Data.Gen ()
import Smos.Default
import System.FileLock
import System.Posix.Terminal
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
            let runOnce = do
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
                  (_, slaveFd) <- openPseudoTerminal
                  let vtyBuilder = do
                        vty <-
                          Vty.mkVty $
                            Vty.defaultConfig
                              { Vty.inputFd = Just slaveFd,
                                Vty.outputFd = Just slaveFd
                              }
                        setWindowSize slaveFd (80, 24)
                        pure vty
                  let runSmos = startSmosWithVtyBuilderOn vtyBuilder startupFile config
                  smosAsync <- async runSmos
                  threadDelay $ 250 * 1000 -- Wait a bit to be sure that smos definitely tried to take the lock
                  mErrOrDone <- poll smosAsync
                  case mErrOrDone of
                    Just (Left err) -> expectationFailure $ displayException err
                    Just (Right ()) -> expectationFailure "Smos exited."
                    Nothing -> pure ()
                  pure smosAsync
            a1 <- runOnce
            cancel a1
            threadDelay $ 250 * 1000 -- Wait a bit to be sure that smos has definitely exited
            res1 <- poll a1
            case res1 of
              Nothing -> expectationFailure "Smos should have exited by now."
              Just (Left e) -> case fromException e of
                Nothing -> expectationFailure "Should not have failed differently."
                Just AsyncCancelled -> pure ()
              Just (Right ()) -> expectationFailure "Should have been canceled, not exited normally."
            a2 <- runOnce
            res2 <- poll a2
            case res2 of
              Nothing -> pure ()
              Just (Left e) -> expectationFailure $ "Smos two should not have failed, but got this exception: " <> displayException e
              Just (Right ()) -> expectationFailure "Should not have errored."
