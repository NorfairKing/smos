{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Archive.OptParse where

import Control.Arrow (left)
import Control.Monad.Logger
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Time
import OptEnvConf
import Path
import Paths_smos_archive (version)
import Smos.CLI.Logging ()
import Smos.CLI.OptParse
import Smos.Directory.OptParse
import Smos.Report.Filter
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
  exportSetPeriod <- parsePeriod
  exportSetFilter <- parseFileFilterArgs
  exportSetAlsoDeleteOriginals <-
    setting
      [ help "Also delete the originals from the archive",
        switch True,
        long "also-delete-originals",
        value False
      ]
  pure ExportSettings {..}

parsePeriod :: Parser (Maybe Period)
parsePeriod =
  parseBeginEnd
    <|> optional
      ( asum
          [ setting [switch Yesterday, long "yesterday", help "yesterday"],
            setting [switch Today, long "today", help "today"],
            setting [switch Tomorrow, long "tomorrow", help "tomorrow"],
            setting [switch LastWeek, long "last-week", help "last week"],
            setting [switch PastWeek, long "past-week", help "the past week"],
            setting [switch ThisWeek, long "this-week", help "this week"],
            setting [switch ComingWeek, long "coming-week", help "the coming week"],
            setting [switch NextWeek, long "next-week", help "next week"],
            setting [switch LastMonth, long "last-month", help "last month"],
            setting [switch PastMonth, long "past-month", help "the past month"],
            setting [switch ThisMonth, long "this-month", help "this month"],
            setting [switch ComingMonth, long "coming-month", help "the coming month"],
            setting [switch NextMonth, long "next-month", help "next month"],
            setting [switch LastYear, long "last-year", help "last year"],
            setting [switch PastYear, long "past-year", help "the past year"],
            setting [switch ThisYear, long "this-year", help "this year"],
            setting [switch ComingYear, long "coming-year", help "the coming year"],
            setting [switch NextYear, long "next-year", help "next year"],
            setting [switch AllTime, long "all-time", help "all time"]
          ]
      )
  where
    parseBeginEnd :: Parser (Maybe Period)
    parseBeginEnd =
      ( \mb me ->
          case (mb, me) of
            (Nothing, Nothing) -> Nothing
            (Just begin, Nothing) -> Just (BeginOnly begin)
            (Nothing, Just end) -> Just (EndOnly end)
            (Just begin, Just end) -> Just (BeginEnd begin end)
      )
        <$> optional
          ( setting
              [ help "start date (inclusive)",
                option,
                reader $ maybeReader parseBegin,
                long "begin",
                metavar "DAY"
              ]
          )
        <*> optional
          ( setting
              [ help "end time (inclusive)",
                option,
                reader $ maybeReader parseEnd,
                long "end",
                metavar "DAY"
              ]
          )
    parseBegin :: String -> Maybe Day
    parseBegin s = parseLocalDay s
    parseEnd :: String -> Maybe Day
    parseEnd s = addDays 1 <$> parseLocalDay s
    parseLocalDay :: String -> Maybe Day
    parseLocalDay = parseTimeM True defaultTimeLocale "%F"

parseFileFilterArgs :: Parser (Maybe (Filter (Path Rel File)))
parseFileFilterArgs =
  fmap foldFilterOr . NE.nonEmpty
    <$> many
      ( setting
          [ help "A filter to smos files by",
            argument,
            reader $ eitherReader $ left (T.unpack . prettyFilterParseError) . parseProjectFilter . T.pack,
            metavar "FILTER"
          ]
      )

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
