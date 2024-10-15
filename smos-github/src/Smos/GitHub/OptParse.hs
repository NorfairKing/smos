{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.GitHub.OptParse
  ( Instructions (..),
    Dispatch (..),
    ImportSettings (..),
    ImportDestination (..),
    Settings (..),
    getInstructions,
  )
where

import Data.Text (Text)
import OptEnvConf
import Path
import Paths_smos_github
import Smos.CLI.Colour
import Smos.CLI.OptParse
import Smos.Directory.OptParse

getInstructions :: IO Instructions
getInstructions = runSettingsParser version "Smos' GitHub tool"

data Instructions
  = Instructions
      !Dispatch
      !Settings

instance HasParser Instructions where
  settingsParser =
    withSmosConfig $
      Instructions
        <$> settingsParser
        <*> settingsParser

data Dispatch
  = DispatchList
  | DispatchImport !ImportSettings

instance HasParser Dispatch where
  settingsParser =
    commands
      [ command "list" "List relevant GitHub issues" $
          pure DispatchList,
        command "import" "Import a GitHub issue as a smos project" $
          DispatchImport <$> settingsParser
      ]

data ImportSettings = ImportSettings
  { importSetUrl :: !String,
    importSetForce :: !Bool,
    importSetDestination :: !ImportDestination
  }

instance HasParser ImportSettings where
  settingsParser = parseImportSettings

{-# ANN parseImportSettings ("NOCOVER" :: String) #-}
parseImportSettings :: OptEnvConf.Parser ImportSettings
parseImportSettings = do
  importSetUrl <-
    setting
      [ help "The url to the issue to import",
        reader str,
        metavar "URL",
        name "url"
      ]
  importSetForce <-
    setting
      [ help "Overwrite an existing file",
        switch True,
        value False,
        long "force"
      ]
  importSetDestination <- settingsParser
  pure ImportSettings {..}

data ImportDestination = ImportDestination
  { importDestinationFile :: !(Maybe (SomeBase File)),
    importDestinationDirectory :: !(Maybe (SomeBase Dir))
  }

instance HasParser ImportDestination where
  settingsParser = parseImportDestination

{-# ANN parseImportDestination ("NOCOVER" :: String) #-}
parseImportDestination :: OptEnvConf.Parser ImportDestination
parseImportDestination = do
  importDestinationFile <-
    optional $
      setting
        [ help "File to put the resulting project in",
          reader $ maybeReader $ fmap Path.Rel . parseRelFile,
          reader $ maybeReader $ fmap Path.Abs . parseAbsFile,
          option,
          long "file",
          short 'f',
          metavar "FILE_PATH"
        ]
  importDestinationDirectory <-
    optional $
      setting
        [ help "Directory to put the resulting project in",
          reader $ maybeReader $ fmap Path.Rel . parseRelDir,
          reader $ maybeReader $ fmap Path.Abs . parseAbsDir,
          option,
          long "directory",
          short 'd',
          metavar "DIRECTORY_PATH"
        ]
  pure ImportDestination {..}

data Settings = Settings
  { setDirectorySettings :: !DirectorySettings,
    setColourConfig :: !ColourSettings,
    setGitHubOauthToken :: !(Maybe Text)
  }

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: OptEnvConf.Parser Settings
parseSettings = do
  setDirectorySettings <- settingsParser
  setColourConfig <- settingsParser
  let sub = subConfig_ "github" . subEnv_ "github"
  setGitHubOauthToken <-
    optional $
      sub $
        choice
          [ mapIO readSecretTextFile $
              filePathSetting
                [ help "Path to an OAuth token for contacting GitHub",
                  name "oauth-token-file"
                ],
            setting
              [ help "OAuth token for contacting GitHub",
                reader str,
                name "oauth-token",
                short 'g',
                metavar "OAUTH_TOKEN"
              ]
          ]
  pure Settings {..}
