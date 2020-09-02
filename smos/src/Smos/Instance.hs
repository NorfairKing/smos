{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Instance where

import Conduit
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Word
import qualified Graphics.Vty as Vty (Config (..), defaultConfig, mkVty)
import Graphics.Vty.Output.TerminfoBased (setWindowSize)
import Path
import Smos
import System.Posix
import UnliftIO
import UnliftIO.Concurrent
import UnliftIO.Resource

data SmosInstanceHandle
  = SmosInstanceHandle
      { smosInstanceHandleMasterHandle :: !Handle,
        smosInstanceHandleSlaveHandle :: !Handle,
        smosInstanceHandleResizeFd :: Fd,
        smosInstanceHandleAsync :: Async ()
      }

withSmosInstance :: (MonadUnliftIO m, MonadResource m) => SmosConfig -> Path Abs File -> (SmosInstanceHandle -> m a) -> m a
withSmosInstance config startingFile func = do
  (masterFd, slaveFd) <- liftIO openPseudoTerminal
  let smosInstanceHandleResizeFd = slaveFd
  (_, smosInstanceHandleMasterHandle) <- allocate (fdToHandle masterFd) hClose -- TODO make sure to close these
  (_, smosInstanceHandleSlaveHandle) <- allocate (fdToHandle slaveFd) hClose
  let vtyBuilder = do
        vty <-
          Vty.mkVty $
            Vty.defaultConfig
              { Vty.inputFd = Just slaveFd,
                Vty.outputFd = Just slaveFd
              }
        setWindowSize smosInstanceHandleResizeFd (80, 24)
        pure vty
  let runSmos :: MonadIO m => m ()
      runSmos = liftIO $ startSmosWithVtyBuilderOn vtyBuilder startingFile config
  withAsync runSmos $ \smosInstanceHandleAsync ->
    func SmosInstanceHandle {..}

smosInstanceInputSink :: MonadIO m => SmosInstanceHandle -> ConduitT ByteString o m ()
smosInstanceInputSink = sinkHandle . smosInstanceHandleMasterHandle

smosInstanceOutputSource :: MonadIO m => SmosInstanceHandle -> ConduitT i ByteString m ()
smosInstanceOutputSource = sourceHandle . smosInstanceHandleMasterHandle

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

smosInstanceResize :: SmosInstanceHandle -> TerminalSize -> IO ()
smosInstanceResize SmosInstanceHandle {..} TerminalSize {..} =
  setWindowSize smosInstanceHandleResizeFd (fromIntegral terminalWidth, fromIntegral terminalHeight)
