module Smos.Archive (smosArchive) where

import Smos.Archive.Commands
import Smos.Archive.Env
import Smos.Archive.OptParse
import Smos.CLI.Logging

smosArchive :: IO ()
smosArchive = do
  Instructions dispatch settings <- getInstructions
  runFilteredLogger (setLogLevel settings) $
    runA settings $ case dispatch of
      DispatchFile file -> smosArchiveFile file
      DispatchExport exportSets -> smosArchiveExport exportSets
