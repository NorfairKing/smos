{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Archive.OptParse where

import Control.Monad.Logger
import OptEnvConf
import Path
import Paths_smos_archive (version)
import Smos.CLI.OptParse
import Smos.Directory.OptParse
import Smos.Report.Filter
import Smos.Report.OptParse
import Smos.Report.Period

getInstructions :: IO Instructions
getInstructions = runSettingsParser version "Smos' archive tool"

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
  = DispatchFile !(Path Abs File)
  | DispatchExport !ExportSettings

instance HasParser Dispatch where
  settingsParser =
    commands
      [ command "file" "Archive a single file" $
          DispatchFile
            <$> filePathSetting
              [ help "The file to archive",
                argument
              ],
        command "export" "Export (a portion of) an archive" $
          DispatchExport <$> settingsParser,
        defaultCommand "file"
      ]

data ExportSettings = ExportSettings
  { exportSetExportDir :: !(Path Abs Dir),
    exportSetPeriod :: !(Maybe Period),
    exportSetFilter :: !(Maybe (Filter (Path Rel File))),
    exportSetAlsoDeleteOriginals :: !Bool
  }

instance HasParser ExportSettings where
  settingsParser = parseExportSettings

{-# ANN parseExportSettings ("NOCOVER" :: String) #-}
parseExportSettings :: OptEnvConf.Parser ExportSettings
parseExportSettings = do
  exportSetExportDir <-
    directoryPathSetting
      [ help "The directory to export the archive to",
        name "directory"
      ]
  exportSetPeriod <- optional settingsParser
  exportSetFilter <- parseFileFilterArgs
  exportSetAlsoDeleteOriginals <-
    setting
      [ help "Also delete the originals from the archive",
        switch True,
        long "also-delete-originals",
        value False
      ]
  pure ExportSettings {..}

data Settings = Settings
  { setDirectorySettings :: !DirectorySettings,
    setLogLevel :: !LogLevel
  }

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: OptEnvConf.Parser Settings
parseSettings = do
  setDirectorySettings <- settingsParser
  let sub = subConfig_ "archive" . subEnv_ "archive"
  setLogLevel <- sub settingsParser
  pure Settings {..}
