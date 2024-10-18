{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Docs.Site.OptParse
  ( Settings (..),
    getSettings,
  )
where

import Data.Text (Text)
import OptEnvConf
import Paths_smos_docs_site (version)

getSettings :: IO Settings
getSettings = runSettingsParser version "Smos' docs site"

data Settings = Settings
  { settingPort :: !Int,
    settingAPIServerUrl :: !(Maybe Text),
    settingWebServerUrl :: !(Maybe Text),
    settingGoogleAnalyticsTracking :: !(Maybe Text),
    settingGoogleSearchConsoleVerification :: !(Maybe Text)
  }

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: OptEnvConf.Parser Settings
parseSettings = subEnv_ "smos-docs-site" $ withLocalYamlConfig $ do
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
