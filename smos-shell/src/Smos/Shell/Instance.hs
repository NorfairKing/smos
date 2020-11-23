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

withSmosShellInstance :: MonadUnliftIO m => SmosReportConfig -> (TerminalHandle -> m a) -> m a
withSmosShellInstance reportConfig = withTerminal $ \slaveHandle _ ->
  liftIO $ smosShellWith reportConfig slaveHandle slaveHandle
