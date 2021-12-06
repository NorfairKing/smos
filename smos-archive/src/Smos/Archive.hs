module Smos.Archive (smosArchive) where

import Control.Monad.Logger
import Smos.Archive.Commands
import Smos.Archive.Env
import Smos.Archive.OptParse

smosArchive :: IO ()
smosArchive = do
  Instructions dispatch settings <- getInstructions
  runStderrLoggingT $
    filterLogger (\_ ll -> ll >= setLogLevel settings) $
      runA settings $ case dispatch of
        DispatchFile file -> smosArchiveFile file
        DispatchExport exportSets -> smosArchiveExport exportSets
