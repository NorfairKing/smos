{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Single.OptParse
  ( Settings (..),
    getSettings,
  )
where

import qualified Data.Text as T
import OptEnvConf
import Path
import Paths_smos_single (version)
import Smos.CLI.OptParse
import Smos.Data
import Smos.Directory.OptParse

getSettings :: IO Settings
getSettings = runSettingsParser version "Smos' Single-task tool"

data Settings = Settings
  { setTask :: !Header,
    setTaskFile :: !(Maybe (Path Rel File)),
    setWorkflowDirSpec :: !WorkflowDirSpec
  }

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: OptEnvConf.Parser Settings
parseSettings = subEnv_ "smos" $ withSmosConfig $ do
  setTask <-
    setting
      [ help "The task. Pass any number of arguments and they will be interpreted as the task together.",
        argument,
        reader $ eitherReader $ parseHeader . T.pack,
        metavar "TASK"
      ]
  setTaskFile <-
    optional $
      setting
        [ help "The file to put the task in",
          reader $ maybeReader parseRelFile,
          option,
          long "file",
          metavar "FILE_PATH"
        ]
  setWorkflowDirSpec <- settingsParser
  pure Settings {..}
