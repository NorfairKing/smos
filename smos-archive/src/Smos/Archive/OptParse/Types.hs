{-# LANGUAGE OverloadedStrings #-}

module Smos.Archive.OptParse.Types where

import Autodocodec
import Control.Monad.Logger
import Path
import Smos.CLI.Logging ()
import Smos.CLI.OptParse
import Smos.Directory.OptParse.Types
import Smos.Report.Filter
import Smos.Report.Period

data Arguments
  = Arguments
      !Command
      !(FlagsWithConfigFile Flags)

data Command
  = CommandFile !FilePath
  | CommandExport !ExportFlags

data ExportFlags = ExportFlags
  { exportFlagExportDir :: FilePath,
    exportFlagFilter :: !(Maybe (Filter (Path Rel File))),
    exportFlagPeriod :: !(Maybe Period),
    exportFlagAlsoDeleteOriginals :: !(Maybe Bool)
  }

data Flags = Flags
  { flagDirectoryFlags :: !DirectoryFlags,
    flagLogLevel :: !(Maybe LogLevel)
  }

data Environment = Environment
  { envDirectoryEnvironment :: !DirectoryEnvironment,
    envLogLevel :: !(Maybe LogLevel)
  }

data Configuration = Configuration
  { confDirectoryConfiguration :: !DirectoryConfiguration,
    confLogLevel :: !(Maybe LogLevel)
  }

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> objectCodec .= confDirectoryConfiguration
        <*> optionalFieldOrNull "log-level" "Minimal severity of log messages" .= confLogLevel

data Instructions = Instructions !Dispatch !Settings

data Dispatch
  = DispatchFile !(Path Abs File)
  | DispatchExport !ExportSettings

data ExportSettings = ExportSettings
  { exportSetExportDir :: !(Path Abs Dir),
    exportSetPeriod :: !Period,
    exportSetFilter :: !(Maybe (Filter (Path Rel File))),
    exportSetAlsoDeleteOriginals :: !Bool
  }

data Settings = Settings
  { setDirectorySettings :: !DirectorySettings,
    setLogLevel :: !LogLevel
  }
