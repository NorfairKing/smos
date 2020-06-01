module Smos.Archive.OptParse.Types where

import Path
import Smos.Report.Config as Report
import Smos.Report.OptParse.Types as Report

data Flags
  = Flags
      { flagFile :: FilePath,
        flagDirectoryFlags :: !Report.DirectoryFlags
      }
  deriving (Show, Eq)

data Configuration
  = Configuration
      { confDirectoryConfiguration :: !Report.DirectoryConfiguration
      }
  deriving (Show, Eq)

data Environment
  = Environment
      { envDirectoryEnvironment :: !Report.DirectoryEnvironment
      }
  deriving (Show, Eq)

data Settings
  = Settings
      { setFile :: !(Path Abs File),
        setDirectorySettings :: !DirectoryConfig
      }
  deriving (Show, Eq)
