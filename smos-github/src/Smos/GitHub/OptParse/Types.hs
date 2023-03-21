{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.GitHub.OptParse.Types where

import Autodocodec
import Data.Text (Text)
import Path
import Smos.CLI.Colour
import Smos.CLI.OptParse
import Smos.Directory.Config
import Smos.Directory.OptParse.Types

data Arguments
  = Arguments
      !Command
      !(FlagsWithConfigFile Flags)
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
  { flagDirectoryFlags :: !DirectoryFlags,
    flagGitHubOAuthToken :: !(Maybe Text),
    flagGitHubOAuthTokenFile :: !(Maybe FilePath)
  }
  deriving (Show, Eq)

data Configuration = Configuration
  { confDirectorySettingsuration :: !DirectorySettingsuration,
    confColourConfiguration :: !(Maybe ColourConfiguration),
    confGitHubConfiguration :: !(Maybe GitHubConfiguration)
  }
  deriving (Show, Eq)

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> objectCodec .= confDirectorySettingsuration
        <*> colourConfigurationTopLevelObjectCodec .= confColourConfiguration
        <*> optionalFieldOrNull "github" "The github tool configuration" .= confGitHubConfiguration

data GitHubConfiguration = GitHubConfiguration
  { githubConfOAuthToken :: !(Maybe Text),
    githubConfOAuthTokenFile :: !(Maybe FilePath)
  }
  deriving (Show, Eq)

instance HasCodec GitHubConfiguration where
  codec =
    object "GitHubConfiguration" $
      GitHubConfiguration
        <$> optionalFieldOrNull "oauth-token" "Oauth token for accessing the github API" .= githubConfOAuthToken
        <*> optionalFieldOrNull "oauth-token-file" "Oauth token file for accessing the github API" .= githubConfOAuthTokenFile

data Environment = Environment
  { envDirectoryEnvironment :: !DirectoryEnvironment,
    envGitHubOAuthToken :: !(Maybe Text),
    envGitHubOAuthTokenFile :: !(Maybe FilePath)
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
  { setDirectorySettings :: !DirectorySettings,
    setColourConfig :: !ColourSettings,
    setGitHubOauthToken :: !(Maybe Text)
  }
  deriving (Show, Eq)
