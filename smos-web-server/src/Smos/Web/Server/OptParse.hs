{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Web.Server.OptParse (Settings (..), getSettings) where

import Control.Monad.Logger
import Data.Text (Text)
import OptEnvConf
import Path
import Paths_smos_web_server (version)
import Servant.Client
-- For HasParser LogLevel
import Smos.CLI.OptParse ()
import Smos.Client
import Smos.Data

getSettings :: IO Settings
getSettings =
  runSettingsParser version $
    unlines $
      concat
        [ ["Smos' web server", ""],
          readWriteDataVersionsHelpMessage,
          clientVersionsHelpMessage
        ]

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

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: OptEnvConf.Parser Settings
parseSettings = subEnv_ "smos-web-server" $ withLocalYamlConfig $ do
  settingLogLevel <- settingsParser
  settingPort <-
    setting
      [ help "The port to serve web requests on",
        reader auto,
        name "port",
        value 8080,
        metavar "PORT"
      ]
  settingDocsUrl <-
    optional $
      setting
        [ help "The url to the docs site to refer to",
          reader $ maybeReader parseBaseUrl,
          name "docs-url",
          metavar "URL"
        ]
  settingAPIUrl <-
    setting
      [ help "The url for the api to use",
        reader $ maybeReader parseBaseUrl,
        name "api-url",
        metavar "URL"
      ]
  settingWebUrl <-
    setting
      [ help "The url that this web server is served from",
        reader $ maybeReader parseBaseUrl,
        name "web-url",
        metavar "URL"
      ]
  settingDataDir <-
    directoryPathSetting
      [ help "The directory to store workflows during editing",
        name "data-dir",
        value "."
      ]
  settingGoogleAnalyticsTracking <-
    optional $
      setting
        [ help "The Google analytics tracking code",
          reader str,
          name "google-analytics-tracking",
          metavar "CODE"
        ]
  settingGoogleSearchConsoleVerification <-
    optional $
      setting
        [ help "The Google search console verification code",
          reader str,
          name "google-search-console-verification",
          metavar "CODE"
        ]
  pure Settings {..}
