{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Docs.Site.OptParse
  ( Settings (..),
    getSettings,
  )
where

import Control.Monad.Logger
import Data.Text (Text)
import qualified Necrork
import OptEnvConf
import Paths_smos_docs_site (version)
import Smos.CLI.Logging ()

getSettings :: IO Settings
getSettings = runSettingsParser version "Smos' docs site"

data Settings = Settings
  { settingLogLevel :: !LogLevel,
    settingPort :: !Int,
    settingAPIServerUrl :: !(Maybe Text),
    settingWebServerUrl :: !(Maybe Text),
    settingNecrorkNotifierSettings :: !(Maybe Necrork.NotifierSettings),
    settingGoogleAnalyticsTracking :: !(Maybe Text),
    settingGoogleSearchConsoleVerification :: !(Maybe Text)
  }

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: OptEnvConf.Parser Settings
parseSettings = subEnv_ "smos-docs-site" $
  withLocalYamlConfig $ do
    settingLogLevel <- settingsParser
    settingPort <-
      setting
        [ help "The port to serve web requests on",
          reader auto,
          name "port",
          value 8080,
          metavar "PORT"
        ]
    settingAPIServerUrl <-
      optional $
        setting
          [ help "The url for the api to use",
            reader str,
            name "api-url",
            metavar "URL"
          ]
    settingWebServerUrl <-
      optional $
        setting
          [ help "The url for the web server to refer to",
            reader str,
            name "web-url",
            metavar "URL"
          ]
    settingNecrorkNotifierSettings <- optional $ subSettings "necrork"
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
