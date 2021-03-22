{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.GitHub.OptParse.Types where

import Data.Aeson
import Data.Text (Text)
import Smos.Query.Config (ColourConfig)
import Smos.Query.OptParse.Types (ColourConfiguration (..), colourConfigurationKey)
import Smos.Report.Config as Report
import qualified Smos.Report.OptParse.Types as Report
import YamlParse.Applicative

data Arguments = Arguments Command (Report.FlagsWithConfigFile Flags)
  deriving (Show, Eq)

data Command = CommandList
  deriving (Show, Eq)

data Flags = Flags
  { flagDirectoryFlags :: !Report.DirectoryFlags,
    flagGithubOAuthToken :: !(Maybe Text)
  }
  deriving (Show, Eq)

data Configuration = Configuration
  { confDirectoryConfiguration :: !Report.DirectoryConfiguration,
    confColourConfiguration :: !(Maybe ColourConfiguration),
    confGitHubConfiguration :: !(Maybe GitHubConfiguration)
  }
  deriving (Show, Eq)

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

instance YamlSchema Configuration where
  yamlSchema =
    Configuration <$> yamlSchema
      <*> objectParser "ColourConfiguration" (optionalField colourConfigurationKey "The github tool configuration")
      <*> objectParser "Configuration" (optionalField "github" "The github tool configuration")

data GitHubConfiguration = GitHubConfiguration
  { githubConfOAuthToken :: !(Maybe Text)
  }
  deriving (Show, Eq)

instance FromJSON GitHubConfiguration where
  parseJSON = viaYamlSchema

instance YamlSchema GitHubConfiguration where
  yamlSchema =
    objectParser "GithubConfiguration" $
      GitHubConfiguration
        <$> optionalField "oauth-token" "Oauth token for accessing the github API"

data Environment = Environment
  { envDirectoryEnvironment :: !Report.DirectoryEnvironment,
    envGithubOAuthToken :: Maybe Text
  }
  deriving (Show, Eq)

data Instructions = Instructions Dispatch Settings
  deriving (Show, Eq)

data Dispatch = DispatchList
  deriving (Show, Eq)

data Settings = Settings
  { setDirectorySettings :: !Report.DirectoryConfig,
    setColourConfig :: !ColourConfig,
    setGithubOauthToken :: !(Maybe Text)
  }
  deriving (Show, Eq)
