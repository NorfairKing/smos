{-# LANGUAGE OverloadedStrings #-}

module Smos.Notify.OptParse.Types where

import Data.Aeson
import qualified Smos.Report.Config as Report
import qualified Smos.Report.OptParse.Types as Report
import YamlParse.Applicative

data Flags = Flags
  { flagDirectoryFlags :: !Report.DirectoryFlags
  }
  deriving (Show, Eq)

data Configuration = Configuration
  { confDirectoryConfiguration :: !Report.DirectoryConfiguration,
    confNotifyConfiguration :: !(Maybe NotifyConfiguration)
  }
  deriving (Show, Eq)

data Environment = Environment
  { envDirectoryEnvironment :: !Report.DirectoryEnvironment
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
  deriving (Show, Eq)

instance FromJSON NotifyConfiguration where
  parseJSON = viaYamlSchema

instance YamlSchema NotifyConfiguration where
  yamlSchema = pure NotifyConfiguration

data Settings = Settings
  { setDirectorySettings :: !Report.DirectoryConfig
  }
  deriving (Show, Eq)
