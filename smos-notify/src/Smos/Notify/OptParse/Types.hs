{-# LANGUAGE OverloadedStrings #-}

module Smos.Notify.OptParse.Types where

import Control.Monad.Logger
import Data.Aeson
import Path
import qualified Smos.Report.Config as Report
import qualified Smos.Report.OptParse.Types as Report
import Text.Read
import YamlParse.Applicative

data Flags = Flags
  { flagDirectoryFlags :: !Report.DirectoryFlags,
    flagDatabase :: !(Maybe FilePath),
    flagLogLevel :: !(Maybe LogLevel)
  }
  deriving (Show, Eq)

data Environment = Environment
  { envDirectoryEnvironment :: !Report.DirectoryEnvironment,
    envDatabase :: !(Maybe FilePath),
    envLogLevel :: !(Maybe LogLevel)
  }
  deriving (Show, Eq)

data Configuration = Configuration
  { confDirectoryConfiguration :: !Report.DirectoryConfiguration,
    confNotifyConfiguration :: !(Maybe NotifyConfiguration)
  }
  deriving (Show, Eq)

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

instance YamlSchema Configuration where
  yamlSchema =
    objectParser "Configuration" $
      Configuration
        <$> Report.directoryConfigurationObjectParser
        <*> optionalField "notify" "Notification Configuration"

data NotifyConfiguration = NotifyConfiguration
  { notifyConfDatabase :: !(Maybe FilePath),
    notifyConfLogLevel :: !(Maybe LogLevel)
  }
  deriving (Show, Eq)

instance FromJSON NotifyConfiguration where
  parseJSON = viaYamlSchema

instance YamlSchema NotifyConfiguration where
  yamlSchema =
    objectParser "NotifyConfiguration" $
      NotifyConfiguration
        <$> optionalField "database" "Database to store sent notifications in"
        <*> optionalFieldWith "log-level" "Log level" (maybeParser parseLogLevel yamlSchema)

data Settings = Settings
  { setDirectorySettings :: !Report.DirectoryConfig,
    setDatabase :: !(Path Abs File),
    setLogLevel :: !LogLevel
  }
  deriving (Show, Eq)

parseLogLevel :: String -> Maybe LogLevel
parseLogLevel s = readMaybe $ "Level" <> s

renderLogLevel :: LogLevel -> String
renderLogLevel = drop 5 . show
