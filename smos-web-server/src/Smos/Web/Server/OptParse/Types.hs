{-# LANGUAGE OverloadedStrings #-}

module Smos.Web.Server.OptParse.Types where

import Autodocodec
import Control.Monad.Logger
import Data.Text (Text)
import Path
import Servant.Client
import Smos.CLI.Logging ()

data Flags = Flags
  { flagLogLevel :: !(Maybe LogLevel),
    flagPort :: !(Maybe Int),
    flagDocsUrl :: !(Maybe String),
    flagAPIUrl :: !(Maybe String),
    flagWebUrl :: !(Maybe String),
    flagDataDir :: !(Maybe FilePath),
    flagGoogleAnalyticsTracking :: !(Maybe String),
    flagGoogleSearchConsoleVerification :: !(Maybe String)
  }

data Environment = Environment
  { envLogLevel :: !(Maybe LogLevel),
    envPort :: !(Maybe Int),
    envDocsUrl :: !(Maybe String),
    envAPIUrl :: !(Maybe String),
    envWebUrl :: !(Maybe String),
    envDataDir :: !(Maybe FilePath),
    envGoogleAnalyticsTracking :: !(Maybe String),
    envGoogleSearchConsoleVerification :: !(Maybe String)
  }

data Configuration = Configuration
  { confLogLevel :: !(Maybe LogLevel),
    confPort :: !(Maybe Int),
    confDocsUrl :: !(Maybe String),
    confAPIUrl :: !(Maybe String),
    confWebUrl :: !(Maybe String),
    confDataDir :: !(Maybe FilePath),
    confGoogleAnalyticsTracking :: !(Maybe String),
    confGoogleSearchConsoleVerification :: !(Maybe String)
  }

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> optionalFieldOrNull "log-level" "The minimal severity for log messages" .= confLogLevel
        <*> optionalFieldOrNull "port" "The port on which to serve web requests" .= confPort
        <*> optionalFieldOrNull "docs-url" "The url for the documentation site to refer to" .= confDocsUrl
        <*> optionalFieldOrNull "api-url" "The url for the api to use" .= confAPIUrl
        <*> optionalFieldOrNull "web-url" "The url that this web server is served from" .= confWebUrl
        <*> optionalFieldOrNull "data-dir" "The directory to store workflows during editing" .= confDataDir
        <*> optionalFieldOrNull "google-analytics-tracking" "The google analytics tracking code" .= confGoogleAnalyticsTracking
        <*> optionalFieldOrNull "google-search-console-verification" "The google search console verification code" .= confGoogleSearchConsoleVerification

data Settings = Settings
  { settingLogLevel :: !LogLevel,
    settingPort :: !Int,
    settingDocsUrl :: !(Maybe BaseUrl),
    settingAPIUrl :: !BaseUrl,
    settingWebUrl :: !BaseUrl,
    settingDataDir :: !(Path Abs Dir),
    settingGoogleAnalyticsTracking :: !(Maybe Text),
    settingGoogleSearchConsoleVerification :: !(Maybe Text)
  }
  deriving (Show)
