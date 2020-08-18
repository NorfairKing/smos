{-# LANGUAGE RecordWildCards #-}

module Smos.ASCIInema.Terminal where

import System.Posix.IO
import System.Posix.Terminal
import System.Posix.Types (Fd)
import UnliftIO

data Terminal
  = Terminal
      { tMasterHandle :: !Handle,
        tSlaveHandle :: !Handle,
        tFd :: !Fd
      }

withPseudoTerminal ::
  MonadUnliftIO m =>
  (Terminal -> m b) ->
  m b
withPseudoTerminal onPseudoTerminal =
  let openPseudoTerminalHandles = liftIO makePseudoTerminal
      closeHandles Terminal {..} = do
        liftIO $ hClose tMasterHandle >> hClose tSlaveHandle
   in bracket openPseudoTerminalHandles closeHandles onPseudoTerminal

makePseudoTerminal :: IO Terminal
makePseudoTerminal = do
  (masterFd, slaveFd) <- openPseudoTerminal
  masterHdl <- fdToHandle masterFd
  slaveHdl <- fdToHandle slaveFd
  do
    attrs <- getTerminalAttributes slaveFd
    let attrs' = withMode attrs EchoErase -- To make sure that backspaces are interpreted correctly
    setTerminalAttributes slaveFd attrs' Immediately
  pure $
    Terminal {tMasterHandle = masterHdl, tSlaveHandle = slaveHdl, tFd = slaveFd}
