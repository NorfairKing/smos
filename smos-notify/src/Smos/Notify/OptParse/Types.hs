{-# LANGUAGE OverloadedStrings #-}

module Smos.Notify.OptParse.Types where

import Autodocodec
import Control.Monad.Logger
import Path
import qualified Smos.Report.Config as Report
import qualified Smos.Report.OptParse.Types as Report
import Text.Read

data Flags = Flags
  { flagDirectoryFlags :: !Report.DirectoryFlags,
    flagDatabase :: !(Maybe FilePath),
    flagNotifySend :: !(Maybe FilePath),
    flagLogLevel :: !(Maybe LogLevel)
  }
  deriving (Show, Eq)

data Environment = Environment
  { envDirectoryEnvironment :: !Report.DirectoryEnvironment,
    envDatabase :: !(Maybe FilePath),
    envNotifySend :: !(Maybe FilePath),
    envLogLevel :: !(Maybe LogLevel)
  }
  deriving (Show, Eq)

data Configuration = Configuration
  { confDirectoryConfiguration :: !Report.DirectoryConfiguration,
    confNotifyConfiguration :: !(Maybe NotifyConfiguration)
  }
  deriving (Show, Eq)

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> objectCodec .= confDirectoryConfiguration
        <*> optionalFieldOrNull "notify" "Notification Configuration" .= confNotifyConfiguration

data NotifyConfiguration = NotifyConfiguration
  { notifyConfDatabase :: !(Maybe FilePath),
    notifyConfNotifySend :: !(Maybe FilePath),
    notifyConfLogLevel :: !(Maybe LogLevel)
  }
  deriving (Show, Eq)

instance HasCodec NotifyConfiguration where
  codec =
    object "NotifyConfiguration" $
      NotifyConfiguration
        <$> optionalFieldOrNull "database" "Database to store sent notifications in" .= notifyConfDatabase
        <*> optionalField "notify-send" "Path to notify-send executable" .= notifyConfNotifySend
        <*> optionalFieldOrNullWith "log-level" (bimapCodec parseLogLevel renderLogLevel codec) "Log level" .= notifyConfLogLevel

data Settings = Settings
  { setDirectorySettings :: !Report.DirectoryConfig,
    setDatabase :: !(Path Abs File),
    setNotifySend :: !(Path Abs File),
    setLogLevel :: !LogLevel
  }
  deriving (Show, Eq)

parseLogLevel :: String -> Either String LogLevel
parseLogLevel s = case readMaybe $ "Level" <> s of
  Nothing -> Left $ unwords ["Unknown log level: " <> show s]
  Just ll -> Right ll

renderLogLevel :: LogLevel -> String
renderLogLevel = drop 5 . show
