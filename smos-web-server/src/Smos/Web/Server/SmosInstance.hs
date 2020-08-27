{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Web.Server.SmosInstance where

import Conduit
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Graphics.Vty as Vty (Config (..), defaultConfig, mkVty)
import Graphics.Vty.Output.TerminfoBased (setWindowSize)
import Path
import Smos
import Smos.Default
import System.Posix
import UnliftIO

data SmosInstanceHandle
  = SmosInstanceHandle
      { smosInstanceHandleMasterHandle :: !Handle,
        smosInstanceHandleSlaveHandle :: !Handle,
        smosInstanceHandleResizeFd :: Fd,
        smosInstanceHandleAsync :: Async ()
      }

withSmosInstance :: MonadUnliftIO m => Path Abs Dir -> Path Abs File -> (SmosInstanceHandle -> m a) -> m a
withSmosInstance workflowDir startingFile = bracket (makeSmosInstance workflowDir startingFile) destroySmosInstance

makeSmosInstance :: MonadUnliftIO m => Path Abs Dir -> Path Abs File -> m SmosInstanceHandle
makeSmosInstance workflowDir startingFile = do
  (masterFd, slaveFd) <- liftIO openPseudoTerminal
  let smosInstanceHandleResizeFd = slaveFd
  smosInstanceHandleMasterHandle <- liftIO $ fdToHandle masterFd
  smosInstanceHandleSlaveHandle <- liftIO $ fdToHandle slaveFd
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
  let runSmos = liftIO $ startSmosWithVtyBuilderOn vtyBuilder startingFile config
  smosInstanceHandleAsync <- async runSmos
  mErrOrDone <- poll smosInstanceHandleAsync
  case mErrOrDone of
    Just (Left err) -> throwIO err
    Just (Right ()) -> throwString "Smos exited."
    Nothing -> pure ()
  pure SmosInstanceHandle {..}

destroySmosInstance :: MonadUnliftIO m => SmosInstanceHandle -> m ()
destroySmosInstance SmosInstanceHandle {..} = do
  liftIO $ putStrLn "Destroying smos instance"
  cancel smosInstanceHandleAsync
  liftIO $ putStrLn "Cancelled the async"

-- For some reason these won't close ...
-- hClose smosInstanceHandleMasterHandle
-- liftIO $ putStrLn "Closed the master"
-- hClose smosInstanceHandleSlaveHandle
-- liftIO $ putStrLn "Closed the slave"

smosInstanceInputSink :: MonadIO m => SmosInstanceHandle -> ConduitT ByteString o m ()
smosInstanceInputSink = sinkHandle . smosInstanceHandleMasterHandle

smosInstanceOutputSource :: MonadIO m => SmosInstanceHandle -> ConduitT i ByteString m ()
smosInstanceOutputSource = sourceHandle . smosInstanceHandleMasterHandle

data TerminalSize
  = TerminalSize
      { terminalWidth :: !Word,
        terminalHeight :: !Word
      }
  deriving (Show)

instance FromJSON TerminalSize where
  parseJSON =
    withObject "TerminalSize" $ \o -> do
      terminalWidth <- o .: "width"
      terminalHeight <- o .: "height"
      pure TerminalSize {..}

smosInstanceResize :: SmosInstanceHandle -> TerminalSize -> IO ()
smosInstanceResize SmosInstanceHandle {..} TerminalSize {..} = setWindowSize smosInstanceHandleResizeFd (fromIntegral terminalWidth, fromIntegral terminalHeight)
