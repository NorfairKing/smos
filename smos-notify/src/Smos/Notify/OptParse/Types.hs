{-# LANGUAGE OverloadedStrings #-}

module Smos.Notify.OptParse.Types where

import Autodocodec
import Control.Monad.Logger
import Path
import Smos.CLI.Logging ()
import Smos.Directory.OptParse.Types

data Flags = Flags
  { flagDirectoryFlags :: !DirectoryFlags,
    flagDatabase :: !(Maybe FilePath),
    flagNotifySend :: !(Maybe FilePath),
    flagLogLevel :: !(Maybe LogLevel)
  }

data Environment = Environment
  { envDirectoryEnvironment :: !DirectoryEnvironment,
    envDatabase :: !(Maybe FilePath),
    envNotifySend :: !(Maybe FilePath),
    envLogLevel :: !(Maybe LogLevel)
  }

data Configuration = Configuration
  { confDirectoryConfiguration :: !DirectoryConfiguration,
    confNotifyConfiguration :: !(Maybe NotifyConfiguration)
  }

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

instance HasCodec NotifyConfiguration where
  codec =
    object "NotifyConfiguration" $
      NotifyConfiguration
        <$> optionalFieldOrNull "database" "Database to store sent notifications in" .= notifyConfDatabase
        <*> optionalField "notify-send" "Path to notify-send executable" .= notifyConfNotifySend
        <*> optionalFieldOrNull "log-level" "Log level" .= notifyConfLogLevel

data Settings = Settings
  { setDirectorySettings :: !DirectorySettings,
    setDatabase :: !(Path Abs File),
    setNotifySend :: !(Path Abs File),
    setLogLevel :: !LogLevel
  }
