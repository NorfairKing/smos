{-# LANGUAGE OverloadedStrings #-}

module Smos.Archive.OptParse.Types where

import Autodocodec
import Control.Monad.Logger
import Path
import Smos.CLI.OptParse
import Smos.Directory.OptParse.Types
import Smos.Report.Filter
import Smos.Report.Period
import Text.Read

data Arguments
  = Arguments
      !Command
      !(FlagsWithConfigFile Flags)
  deriving (Show, Eq)

data Command
  = CommandFile !FilePath
  | CommandExport !ExportFlags
  deriving (Show, Eq)

data ExportFlags = ExportFlags
  { exportFlagExportDir :: FilePath,
    exportFlagFilter :: !(Maybe (Filter (Path Rel File))),
    exportFlagPeriod :: !(Maybe Period),
    exportFlagAlsoDeleteOriginals :: !(Maybe Bool)
  }
  deriving (Show, Eq)

data Flags = Flags
  { flagDirectoryFlags :: !DirectoryFlags,
    flagLogLevel :: !(Maybe LogLevel)
  }
  deriving (Show, Eq)

data Environment = Environment
  { envDirectoryEnvironment :: !DirectoryEnvironment,
    envLogLevel :: !(Maybe LogLevel)
  }
  deriving (Show, Eq)

data Configuration = Configuration
  { confDirectoryConfiguration :: !DirectoryConfiguration,
    confLogLevel :: !(Maybe LogLevel)
  }
  deriving (Show, Eq)

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> objectCodec .= confDirectoryConfiguration
        <*> optionalFieldOrNullWith
          "log-level"
          (bimapCodec parseLogLevel renderLogLevel codec)
          "Minimal severity of log messages"
          .= confLogLevel

data Instructions = Instructions !Dispatch !Settings
  deriving (Show, Eq)

data Dispatch
  = DispatchFile !(Path Abs File)
  | DispatchExport !ExportSettings
  deriving (Show, Eq)

data ExportSettings = ExportSettings
  { exportSetExportDir :: !(Path Abs Dir),
    exportSetPeriod :: !Period,
    exportSetFilter :: !(Maybe (Filter (Path Rel File))),
    exportSetAlsoDeleteOriginals :: !Bool
  }
  deriving (Show, Eq)

data Settings = Settings
  { setDirectorySettings :: !DirectorySettings,
    setLogLevel :: !LogLevel
  }
  deriving (Show, Eq)

parseLogLevel :: String -> Either String LogLevel
parseLogLevel s = case readMaybe $ "Level" <> s of
  Nothing -> Left $ unwords ["Unknown log level: " <> show s]
  Just ll -> Right ll

renderLogLevel :: LogLevel -> String
renderLogLevel = drop 5 . show
