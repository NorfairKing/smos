{-# LANGUAGE RecordWildCards #-}

module Smos.Web.Server.SmosInstance where

import Conduit
import Control.Concurrent
import Data.ByteString (ByteString)
import qualified Graphics.Vty as Vty (Config (..), defaultConfig, mkVty)
import Graphics.Vty.Output.TerminfoBased (setWindowSize)
import Path
import Smos
import Smos.Default
import System.Posix
import System.Posix.IO
import System.Posix.Terminal
import UnliftIO

data SmosInstanceHandle
  = SmosInstanceHandle
      { smosInstanceHandleMasterHandle :: !Handle,
        smosInstanceHandleSlaveHandle :: !Handle,
        smosInstanceHandleResizeFd :: Fd,
        smosInstanceHandleAsync :: Async ()
      }

makeSmosInstance :: Path Abs Dir -> Path Abs File -> IO SmosInstanceHandle
makeSmosInstance workflowDir startingFile = do
  (masterFd, slaveFd) <- openPseudoTerminal
  let smosInstanceHandleResizeFd = slaveFd
  smosInstanceHandleMasterHandle <- fdToHandle masterFd
  smosInstanceHandleSlaveHandle <- fdToHandle slaveFd
  let vtyBuilder = do
        vty <- Vty.mkVty $ Vty.defaultConfig {Vty.inputFd = Just slaveFd, Vty.outputFd = Just slaveFd}
        setWindowSize smosInstanceHandleResizeFd (80, 24)
        pure vty
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
  let runSmos = startSmosWithVtyBuilderOn vtyBuilder startingFile config
  smosInstanceHandleAsync <- async runSmos
  -- Wait a bit to be sure that smos did the initialisation
  -- TODO get rid of this. The websockets should take care of it
  threadDelay $ 100 * 1000
  mErrOrDone <- poll smosInstanceHandleAsync
  case mErrOrDone of
    Just (Left err) -> fail $ displayException err
    Just (Right ()) -> fail "Smos exited."
    Nothing -> pure ()
  pure SmosInstanceHandle {..}

destroySmosInstance :: SmosInstanceHandle -> IO ()
destroySmosInstance SmosInstanceHandle {..} = do
  hClose smosInstanceHandleMasterHandle
  hClose smosInstanceHandleSlaveHandle
  cancel smosInstanceHandleAsync

smosInstanceInputSink :: MonadIO m => SmosInstanceHandle -> ConduitT ByteString o m ()
smosInstanceInputSink = sinkHandle . smosInstanceHandleMasterHandle

smosInstanceOutputSource :: MonadIO m => SmosInstanceHandle -> ConduitT i ByteString m ()
smosInstanceOutputSource = sourceHandle . smosInstanceHandleMasterHandle

-- TODO use a struct to deal with the width and height so that we can never mix up width and height
smosInstanceResize :: SmosInstanceHandle -> Word -> Word -> IO ()
smosInstanceResize SmosInstanceHandle {..} width height = setWindowSize smosInstanceHandleResizeFd (fromIntegral width, fromIntegral height)
