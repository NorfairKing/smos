{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Scheduler.OptParse
  ( Instructions (..),
    Dispatch (..),
    Settings (..),
    getInstructions,
  )
where

import qualified Data.Map as M
import Data.Validity.Path ()
import OptEnvConf
import Path
import Paths_smos_scheduler (version)
import Smos.CLI.Colour
import Smos.CLI.OptParse
import Smos.Data
import Smos.Directory.OptParse
import Smos.Scheduler.Schedule

getInstructions :: IO Instructions
getInstructions =
  runSettingsParser version $
    unlines $
      concat
        [ [ "Smos' scheduler tool",
            ""
          ],
          writeDataVersionsHelpMessage
        ]

data Instructions = Instructions Dispatch Settings

instance HasParser Instructions where
  settingsParser =
    withSmosConfig $
      Instructions
        <$> settingsParser
        <*> settingsParser

data Dispatch
  = DispatchCheck
  | DispatchSample !(Path Abs File) !(Maybe DestinationPathTemplate)
  | DispatchSchedule
  | DispatchNext

instance HasParser Dispatch where
  settingsParser =
    commands
      [ command "check" "Check that all schedules are valid" $
          pure DispatchCheck,
        command "sample" "Produce a sample scheduled project being filled in" $
          DispatchSample
            <$> filePathSetting
              [ help "template to fill in",
                argument
              ]
            <*> optional
              ( setting
                  [ help "destination path template. Note that the rendered template will be written here",
                    reader $ maybeReader $ fmap DestinationPathTemplate . parseRelFile,
                    option,
                    long "destination",
                    metavar "PATH_TEMPLATE"
                  ]
              ),
        command "schedule" "Run the schedules" $
          pure DispatchSchedule,
        command "next" "List the next times that scheduled will be activated" $
          pure DispatchNext
      ]

data Settings = Settings
  { setDirectorySettings :: !DirectorySettings,
    setSchedule :: !Schedule,
    setColourSettings :: !ColourSettings
  }

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: OptEnvConf.Parser Settings
parseSettings = do
  setDirectorySettings <- settingsParser
  setColourSettings <- settingsParser
  let sub = subConfig_ "scheduler" . subEnv_ "scheduler"
  setSchedule <-
    sub $
      Schedule
        <$> setting
          [ help "Schedule on which to schedule projects",
            conf "schedule",
            value M.empty
          ]
  pure Settings {..}
