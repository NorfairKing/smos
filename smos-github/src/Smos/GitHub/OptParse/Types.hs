{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.GitHub.OptParse.Types where

import Data.Aeson
import Smos.Report.Config as Report
import qualified Smos.Report.OptParse.Types as Report
import YamlParse.Applicative

data Arguments = Arguments Command (Report.FlagsWithConfigFile Flags)
  deriving (Show, Eq)

data Command = CommandList
  deriving (Show, Eq)

data Flags = Flags
  { flagDirectoryFlags :: !Report.DirectoryFlags
  }
  deriving (Show, Eq)

data Configuration = Configuration
  { confDirectoryConfiguration :: !Report.DirectoryConfiguration,
    confGitHubConfiguration :: !(Maybe GitHubConfiguration)
  }
  deriving (Show, Eq)

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

instance YamlSchema Configuration where
  yamlSchema = Configuration <$> yamlSchema <*> objectParser "Configuration" (optionalField "scheduler" "The scheduler configuration")

data GitHubConfiguration = GitHubConfiguration
  {
  }
  deriving (Show, Eq)

instance FromJSON GitHubConfiguration where
  parseJSON = viaYamlSchema

instance YamlSchema GitHubConfiguration where
  yamlSchema = pure GitHubConfiguration

data Environment = Environment
  { envDirectoryEnvironment :: !Report.DirectoryEnvironment
  }
  deriving (Show, Eq)

data Instructions = Instructions Dispatch Settings
  deriving (Show, Eq)

data Dispatch = DispatchList
  deriving (Show, Eq)

data Settings = Settings
  { setDirectorySettings :: !Report.DirectoryConfig
  }
  deriving (Show, Eq)
