{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Instance where

import Conduit
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Word
import qualified Graphics.Vty as Vty (Config (..), defaultConfig, mkVty)
import Graphics.Vty.Output.TerminfoBased (setWindowSize)
import Smos
import Smos.Types
import System.Posix
import UnliftIO

data SmosInstanceHandle
  = SmosInstanceHandle
      { smosInstanceHandleMasterHandle :: !Handle,
        smosInstanceHandleSlaveHandle :: !Handle,
        smosInstanceHandleResizeFd :: Fd,
        smosInstanceHandleAsync :: Async ()
      }

withSmosInstance :: MonadUnliftIO m => SmosConfig -> Maybe StartingPath -> (SmosInstanceHandle -> m a) -> m a
withSmosInstance config mStartingPath func = do
  (masterFd, slaveFd) <- liftIO openPseudoTerminal
  let smosInstanceHandleResizeFd = slaveFd
  smosInstanceHandleMasterHandle <- liftIO $ fdToHandle masterFd
  flip finally (hClose smosInstanceHandleMasterHandle) $ do
    smosInstanceHandleSlaveHandle <- liftIO $ fdToHandle slaveFd
    flip finally (hClose smosInstanceHandleSlaveHandle) $ do
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
          runSmos = liftIO $ startSmosWithVtyBuilderOn vtyBuilder mStartingPath config
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
