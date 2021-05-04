module Smos.Shell.Instance
  ( module Smos.Shell.Instance,
    module Smos.Terminal,
  )
where

import Smos.Report.Config
import Smos.Shell
import Smos.Terminal
import UnliftIO

type SmosShellInstanceHandle = TerminalHandle

withSmosShellInstance :: MonadUnliftIO m => DirectoryConfig -> (TerminalHandle -> m a) -> m a
withSmosShellInstance directoryConfig = withTerminal $ \slaveHandle _ ->
  liftIO $ smosShellWith directoryConfig slaveHandle slaveHandle slaveHandle
