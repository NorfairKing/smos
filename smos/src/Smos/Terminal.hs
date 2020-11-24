{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Terminal where

import Conduit
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Word
import Graphics.Vty.Output.TerminfoBased (setWindowSize)
import System.Posix
import UnliftIO

data TerminalHandle
  = TerminalHandle
      { terminalHandleMasterHandle :: !Handle,
        terminalHandleSlaveHandle :: !Handle,
        terminalHandleResizeFd :: Fd,
        terminalHandleAsync :: Async ()
      }

withTerminal :: MonadUnliftIO m => (Handle -> Fd -> m ()) -> (TerminalHandle -> m a) -> m a
withTerminal programFunc func = do
  (masterFd, slaveFd) <- liftIO openPseudoTerminal
  let terminalHandleResizeFd = slaveFd
  terminalHandleMasterHandle <- liftIO $ fdToHandle masterFd
  flip finally (hClose terminalHandleMasterHandle) $ do
    terminalHandleSlaveHandle <- liftIO $ fdToHandle slaveFd
    flip finally (hClose terminalHandleSlaveHandle) $ do
      let runProgram = programFunc terminalHandleSlaveHandle terminalHandleResizeFd
      withAsync runProgram $ \terminalHandleAsync -> do
        liftIO $ setWindowSize terminalHandleResizeFd (80, 24)
        let th = TerminalHandle {..}
        func th

terminalInputSink :: MonadIO m => TerminalHandle -> ConduitT ByteString o m ()
terminalInputSink = sinkHandle . terminalHandleMasterHandle

terminalOutputSource :: MonadIO m => TerminalHandle -> ConduitT i ByteString m ()
terminalOutputSource = sourceHandle . terminalHandleMasterHandle

data TerminalSize
  = TerminalSize
      { terminalWidth :: !Word16,
        terminalHeight :: !Word16
      }
  deriving (Show)

instance FromJSON TerminalSize where
  parseJSON =
    withObject "TerminalSize" $ \o -> do
      terminalWidth <- o .: "width"
      terminalHeight <- o .: "height"
      pure TerminalSize {..}

terminalResize :: TerminalHandle -> TerminalSize -> IO ()
terminalResize TerminalHandle {..} TerminalSize {..} =
  setWindowSize terminalHandleResizeFd (fromIntegral terminalWidth, fromIntegral terminalHeight)
