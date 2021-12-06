module Smos.Archive (smosArchive) where

import Control.Monad.Reader
import Smos.Archive.Commands
import Smos.Archive.OptParse

smosArchive :: IO ()
smosArchive = do
  Instructions dispatch settings <- getInstructions
  case dispatch of
    DispatchFile file -> runReaderT (smosArchiveFile file) settings
