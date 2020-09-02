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

data SmosInstanceHandle
  = SmosInstanceHandle
      { smosInstanceHandleMasterHandle :: !Handle,
        smosInstanceHandleSlaveHandle :: !Handle,
        smosInstanceHandleResizeFd :: Fd,
        smosInstanceHandleAsync :: Async ()
      }

withSmosInstance :: MonadUnliftIO m => SmosConfig -> Path Abs File -> (SmosInstanceHandle -> m a) -> m a
withSmosInstance config startingFile = bracket (makeSmosInstance config startingFile) destroySmosInstance

makeSmosInstance :: MonadUnliftIO m => SmosConfig -> Path Abs File -> m SmosInstanceHandle
makeSmosInstance config startingFile = do
  (masterFd, slaveFd) <- liftIO openPseudoTerminal
  let smosInstanceHandleResizeFd = slaveFd
  smosInstanceHandleMasterHandle <- liftIO $ fdToHandle masterFd
  smosInstanceHandleSlaveHandle <- liftIO $ fdToHandle slaveFd
  let vtyBuilder = do
        vty <-
          Vty.mkVty $
            Vty.defaultConfig
              { Vty.inputFd = Just slaveFd,
                Vty.outputFd = Just slaveFd
              }
        setWindowSize smosInstanceHandleResizeFd (80, 24)
        pure vty
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
  cancel smosInstanceHandleAsync

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
