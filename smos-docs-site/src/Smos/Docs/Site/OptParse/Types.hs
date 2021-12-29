{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Docs.Site.OptParse.Types where

import Autodocodec
import Data.Text (Text)
import GHC.Generics (Generic)

data Flags = Flags
  { flagConfigFile :: !(Maybe FilePath),
    flagPort :: !(Maybe Int),
    flagAPIServerUrl :: !(Maybe String),
    flagWebServerUrl :: !(Maybe String),
    flagGoogleAnalyticsTracking :: !(Maybe String),
    flagGoogleSearchConsoleVerification :: !(Maybe String)
  }
  deriving (Show, Eq)

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

data Settings = Settings
  { settingPort :: !Int,
    settingAPIServerUrl :: !(Maybe Text),
    settingWebServerUrl :: !(Maybe Text),
    settingGoogleAnalyticsTracking :: !(Maybe Text),
    settingGoogleSearchConsoleVerification :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)
