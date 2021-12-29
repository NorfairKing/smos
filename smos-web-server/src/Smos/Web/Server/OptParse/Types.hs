{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Web.Server.OptParse.Types where

import Autodocodec
import Control.Monad.Logger
import Data.Text (Text)
import GHC.Generics (Generic)
import Path
import Servant.Client
import Text.Read

data Flags = Flags
  { flagConfigFile :: !(Maybe FilePath),
    flagLogLevel :: !(Maybe LogLevel),
    flagPort :: !(Maybe Int),
    flagDocsUrl :: !(Maybe String),
    flagAPIUrl :: !(Maybe String),
    flagWebUrl :: !(Maybe String),
    flagDataDir :: !(Maybe FilePath),
    flagGoogleAnalyticsTracking :: !(Maybe String),
    flagGoogleSearchConsoleVerification :: !(Maybe String)
  }
  deriving (Show, Eq)

data Environment = Environment
  { envConfigFile :: !(Maybe FilePath),
    envLogLevel :: !(Maybe LogLevel),
    envPort :: !(Maybe Int),
    envDocsUrl :: !(Maybe String),
    envAPIUrl :: !(Maybe String),
    envWebUrl :: !(Maybe String),
    envDataDir :: !(Maybe FilePath),
    envGoogleAnalyticsTracking :: !(Maybe String),
    envGoogleSearchConsoleVerification :: !(Maybe String)
  }
  deriving (Show, Eq, Generic)

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
  deriving (Show, Eq, Generic)

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> optionalFieldOrNullWith "log-level" (bimapCodec parseLogLevel renderLogLevel codec) "The minimal severity for log messages" .= confLogLevel
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
  deriving (Show, Eq, Generic)

parseLogLevel :: String -> Either String LogLevel
parseLogLevel s = case readMaybe $ "Level" <> s of
  Nothing -> Left $ unwords ["Unknown log level: " <> show s]
  Just ll -> Right ll

renderLogLevel :: LogLevel -> String
renderLogLevel = drop 5 . show
