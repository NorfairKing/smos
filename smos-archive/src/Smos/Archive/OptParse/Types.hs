module Smos.Archive.OptParse.Types where

import Path
import Smos.Report.Config as Report
import Smos.Report.OptParse.Types as Report

data Flags
  = Flags
      { flagFile :: FilePath,
        flagReportFlags :: Report.Flags
      }
  deriving (Show, Eq)

newtype Configuration
  = Configuration
      { confReportConfiguration :: Report.Configuration
      }
  deriving (Show, Eq)

newtype Environment
  = Environment
      { envReportEnvironment :: Report.Environment
      }
  deriving (Show, Eq)

data Settings
  = Settings
      { setFile :: Path Abs File,
        setReportSettings :: SmosReportConfig
      }
  deriving (Show, Eq)
