{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Docs.Site.OptParse.Types where

import Data.Text (Text)
import Data.Yaml as Yaml
import GHC.Generics (Generic)
import YamlParse.Applicative

data Arguments
  = Arguments Command Flags
  deriving (Show, Eq)

data Instructions
  = Instructions Dispatch Settings

newtype Command
  = CommandServe ServeFlags
  deriving (Show, Eq)

data ServeFlags = ServeFlags
  { serveFlagPort :: !(Maybe Int),
    serveFlagAPIServerUrl :: !(Maybe String),
    serveFlagWebServerUrl :: !(Maybe String),
    serveFlagGoogleAnalyticsTracking :: !(Maybe String),
    serveFlagGoogleSearchConsoleVerification :: !(Maybe String)
  }
  deriving (Show, Eq)

data Flags = Flags
  { flagConfigFile :: !(Maybe FilePath)
  }
  deriving (Show, Eq, Generic)

data Environment = Environment
  { envConfigFile :: !(Maybe FilePath),
    envPort :: !(Maybe Int),
    envAPIServerUrl :: !(Maybe String),
    envWebServerUrl :: !(Maybe String),
    envGoogleAnalyticsTracking :: !(Maybe String),
    envGoogleSearchConsoleVerification :: !(Maybe String)
  }
  deriving (Show, Eq, Generic)

data Configuration = Configuration
  { confPort :: !(Maybe Int),
    confAPIServerUrl :: !(Maybe String),
    confWebServerUrl :: !(Maybe String),
    confGoogleAnalyticsTracking :: !(Maybe String),
    confGoogleSearchConsoleVerification :: !(Maybe String)
  }
  deriving (Show, Eq, Generic)

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

instance YamlSchema Configuration where
  yamlSchema =
    objectParser "Configuration" $
      Configuration
        <$> optionalField "port" "The port on which to serve web requests"
        <*> optionalField "api-url" "The url for the api server to rever to"
        <*> optionalField "web-url" "The url for the web server to refer to"
        <*> optionalField "google-analytics-tracking" "The google analytics tracking code"
        <*> optionalField "google-search-console-verification" "The google search console verification code"

newtype Dispatch
  = DispatchServe ServeSettings
  deriving (Show, Eq, Generic)

data ServeSettings = ServeSettings
  { serveSetPort :: !Int,
    serveSetAPIServerUrl :: !(Maybe Text),
    serveSetWebServerUrl :: !(Maybe Text),
    serveSetGoogleAnalyticsTracking :: !(Maybe Text),
    serveSetGoogleSearchConsoleVerification :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

data Settings
  = Settings
  deriving (Show, Eq, Generic)
