{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.GitHub.OptParse.Types where

import Autodocodec
import Data.Text (Text)
import Path
import Smos.CLI.Colour
import Smos.CLI.OptParse
import Smos.Directory.OptParse.Types

data Arguments
  = Arguments
      !Command
      !(FlagsWithConfigFile Flags)

data Command
  = CommandList
  | CommandImport !ImportFlags

data ImportFlags = ImportFlags
  { importFlagUrl :: !String,
    importFlagForce :: !Bool,
    importFlagFile :: !(Maybe FilePath),
    importFlagDirectory :: !(Maybe FilePath)
  }

data Flags = Flags
  { flagDirectoryFlags :: !DirectoryFlags,
    flagGitHubOAuthToken :: !(Maybe Text),
    flagGitHubOAuthTokenFile :: !(Maybe FilePath)
  }

data Configuration = Configuration
  { confDirectoryConfiguration :: !DirectoryConfiguration,
    confColourConfiguration :: !(Maybe ColourConfiguration),
    confGitHubConfiguration :: !(Maybe GitHubConfiguration)
  }

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> objectCodec .= confDirectoryConfiguration
        <*> colourConfigurationTopLevelObjectCodec .= confColourConfiguration
        <*> optionalFieldOrNull "github" "The github tool configuration" .= confGitHubConfiguration

data GitHubConfiguration = GitHubConfiguration
  { githubConfOAuthToken :: !(Maybe Text),
    githubConfOAuthTokenFile :: !(Maybe FilePath)
  }

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

data Instructions
  = Instructions
      !Dispatch
      !Settings

data Dispatch
  = DispatchList
  | DispatchImport !ImportSettings

data ImportSettings = ImportSettings
  { importSetUrl :: !String,
    importSetForce :: !Bool,
    importSetDestination :: !ImportDestination
  }

data ImportDestination = ImportDestination
  { importDestinationFile :: !(Maybe (Path.SomeBase File)),
    importDestinationDirectory :: !(Maybe (Path.SomeBase Dir))
  }

data Settings = Settings
  { setDirectorySettings :: !DirectorySettings,
    setColourConfig :: !ColourSettings,
    setGitHubOauthToken :: !(Maybe Text)
  }
