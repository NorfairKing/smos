module Smos.Instance
  ( module Smos.Instance,
    module Smos.Terminal,
  )
where

import Conduit
import Data.ByteString (ByteString)
import qualified Graphics.Vty as Vty (Config (..), defaultConfig, mkVty)
import Graphics.Vty.Output.TerminfoBased (setWindowSize)
import Smos
import Smos.Terminal
import Smos.Types
import System.Posix
import UnliftIO

type SmosInstanceHandle = TerminalHandle

withSmosInstance :: MonadUnliftIO m => SmosConfig -> Maybe StartingPath -> (SmosInstanceHandle -> m a) -> m a
withSmosInstance config mStartingPath = withTerminal $ \_ slaveFd ->
  let vtyBuilder = do
        vty <-
          Vty.mkVty $
            Vty.defaultConfig
              { Vty.inputFd = Just slaveFd,
                Vty.outputFd = Just slaveFd
              }
        setWindowSize slaveFd (80, 24)
        pure vty
      runSmos :: MonadIO m => m ()
      runSmos = liftIO $ startSmosWithVtyBuilderOn vtyBuilder mStartingPath config
   in runSmos

smosInstanceHandleMasterHandle :: SmosInstanceHandle -> Handle
smosInstanceHandleMasterHandle = terminalHandleMasterHandle

smosInstanceHandleSlaveHandle :: SmosInstanceHandle -> Handle
smosInstanceHandleSlaveHandle = terminalHandleSlaveHandle

smosInstanceHandleResizeFd :: SmosInstanceHandle -> Fd
smosInstanceHandleResizeFd = terminalHandleResizeFd

smosInstanceHandleAsync :: SmosInstanceHandle -> Async ()
smosInstanceHandleAsync = terminalHandleAsync

smosInstanceInputSink :: MonadIO m => SmosInstanceHandle -> ConduitT ByteString o m ()
smosInstanceInputSink = sinkHandle . smosInstanceHandleMasterHandle

smosInstanceOutputSource :: MonadIO m => SmosInstanceHandle -> ConduitT i ByteString m ()
smosInstanceOutputSource = sourceHandle . smosInstanceHandleMasterHandle

smosInstanceResize :: SmosInstanceHandle -> TerminalSize -> IO ()
smosInstanceResize = terminalResize
