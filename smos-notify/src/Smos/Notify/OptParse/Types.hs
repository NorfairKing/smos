{-# LANGUAGE OverloadedStrings #-}

module Smos.Notify.OptParse.Types where

import Data.Aeson
import Path
import qualified Smos.Report.Config as Report
import qualified Smos.Report.OptParse.Types as Report
import YamlParse.Applicative

data Flags = Flags
  { flagDirectoryFlags :: !Report.DirectoryFlags,
    flagDatabase :: !(Maybe FilePath)
  }
  deriving (Show, Eq)

data Environment = Environment
  { envDirectoryEnvironment :: !Report.DirectoryEnvironment,
    envDatabase :: !(Maybe FilePath)
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
  { notifyConfDatabase :: !(Maybe FilePath)
  }
  deriving (Show, Eq)

instance FromJSON NotifyConfiguration where
  parseJSON = viaYamlSchema

instance YamlSchema NotifyConfiguration where
  yamlSchema =
    objectParser "NotifyConfiguration" $
      NotifyConfiguration
        <$> optionalField "database" "Database to store sent notifications in"

data Settings = Settings
  { setDirectorySettings :: !Report.DirectoryConfig,
    setDatabase :: !(Path Abs File)
  }
  deriving (Show, Eq)
