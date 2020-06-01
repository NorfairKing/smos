module Smos.Single.OptParse.Types where

import Path
import Smos.Data
import Smos.Report.Config as Report
import Smos.Report.OptParse.Types as Report

data Flags
  = Flags
      { flagTaskPieces :: ![String],
        flagTaskFile :: !(Maybe FilePath),
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
      { setTask :: !Header,
        setTaskFile :: !(Maybe (Path Rel File)),
        setDirectorySettings :: !DirectoryConfig
      }
  deriving (Show, Eq)
