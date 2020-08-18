{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.ASCIInema.Terminal where

import System.Posix.IO
import System.Posix.Terminal
import System.Posix.Types (Fd)
import UnliftIO

data Terminal
  = Terminal
      { tMasterHandle :: !Handle,
        tSlaveHandle :: !Handle,
        tFd :: !Fd,
        tAttributes :: !TerminalAttributes
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

deriving instance Show TerminalMode

makePseudoTerminal :: IO Terminal
makePseudoTerminal = do
  (masterFd, slaveFd) <- openPseudoTerminal
  masterHdl <- fdToHandle masterFd
  slaveHdl <- fdToHandle slaveFd
  attrs <- getTerminalAttributes slaveFd
  pure $
    Terminal {tMasterHandle = masterHdl, tSlaveHandle = slaveHdl, tFd = slaveFd, tAttributes = attrs}
