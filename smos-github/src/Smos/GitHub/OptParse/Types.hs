{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.GitHub.OptParse.Types where

import Autodocodec
import Data.Text (Text)
import Path
import Smos.Query.OptParse.Types (ColourConfiguration (..), ColourSettings, colourConfigurationKey)
import Smos.Report.Config as Report
import qualified Smos.Report.OptParse.Types as Report

data Arguments
  = Arguments
      !Command
      !(Report.FlagsWithConfigFile Flags)
  deriving (Show, Eq)

data Command
  = CommandList
  | CommandImport !ImportFlags
  deriving (Show, Eq)

data ImportFlags = ImportFlags
  { importFlagUrl :: !String,
    importFlagForce :: !Bool,
    importFlagFile :: !(Maybe FilePath),
    importFlagDirectory :: !(Maybe FilePath)
  }
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

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> objectCodec .= confDirectoryConfiguration
        <*> optionalFieldOrNull colourConfigurationKey "The colour configuration" .= confColourConfiguration
        <*> optionalFieldOrNull "github" "The github tool configuration" .= confGitHubConfiguration

data GitHubConfiguration = GitHubConfiguration
  { githubConfOAuthToken :: !(Maybe Text)
  }
  deriving (Show, Eq)

instance HasCodec GitHubConfiguration where
  codec =
    object "GithubConfiguration" $
      GitHubConfiguration
        <$> optionalFieldOrNull "oauth-token" "Oauth token for accessing the github API" .= githubConfOAuthToken

data Environment = Environment
  { envDirectoryEnvironment :: !Report.DirectoryEnvironment,
    envGithubOAuthToken :: !(Maybe Text)
  }
  deriving (Show, Eq)

data Instructions
  = Instructions
      !Dispatch
      !Settings
  deriving (Show, Eq)

data Dispatch
  = DispatchList
  | DispatchImport !ImportSettings
  deriving (Show, Eq)

data ImportSettings = ImportSettings
  { importSetUrl :: !String,
    importSetForce :: !Bool,
    importSetDestination :: !ImportDestination
  }
  deriving (Show, Eq)

data ImportDestination = ImportDestination
  { importDestinationFile :: !(Maybe (Path.SomeBase File)),
    importDestinationDirectory :: !(Maybe (Path.SomeBase Dir))
  }
  deriving (Show, Eq)

data Settings = Settings
  { setDirectorySettings :: !Report.DirectoryConfig,
    setColourConfig :: !ColourSettings,
    setGithubOauthToken :: !(Maybe Text)
  }
  deriving (Show, Eq)
