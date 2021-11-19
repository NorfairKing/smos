{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Docs.Site.OptParse.Types where

import Autodocodec
import Data.Text (Text)
import GHC.Generics (Generic)

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

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> optionalFieldOrNull "port" "The port on which to serve web requests" .= confPort
        <*> optionalFieldOrNull "api-url" "The url for the api server to rever to" .= confAPIServerUrl
        <*> optionalFieldOrNull "web-url" "The url for the web server to refer to" .= confWebServerUrl
        <*> optionalFieldOrNull "google-analytics-tracking" "The google analytics tracking code" .= confGoogleAnalyticsTracking
        <*> optionalFieldOrNull "google-search-console-verification" "The google search console verification code" .= confGoogleSearchConsoleVerification

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
