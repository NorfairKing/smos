{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Smos.Report.OptParse
  ( module Smos.Report.OptParse,
    module Smos.Report.OptParse.Types,
  )
where

import Control.Applicative
import Control.Arrow
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import qualified Env
import Options.Applicative
import Path
import Smos.Directory.Archive
import Smos.Directory.OptParse
import Smos.Report.Agenda.Types
import Smos.Report.Filter
import Smos.Report.OptParse.Types
import Smos.Report.Period
import Smos.Report.Projection
import Smos.Report.Sorter
import Smos.Report.Time
import Smos.Report.TimeBlock

combineToSettings ::
  ReportSettings -> Flags -> Environment -> Maybe Configuration -> IO ReportSettings
combineToSettings src Flags {..} Environment {..} mc = do
  reportSettingDirectorySettings <- combineToDirectorySettings (reportSettingDirectorySettings src) flagDirectoryFlags envDirectoryEnvironment (confDirectoryConf <$> mc)
  reportSettingWaitingSettings <- combineToWaitingReportSettings (reportSettingWaitingSettings src) (mc >>= confWaitingReportConf)
  reportSettingStuckSettings <- combineToStuckReportSettings (reportSettingStuckSettings src) (mc >>= confStuckReportConf)
  reportSettingWorkSettings <- combineToWorkReportSettings (reportSettingWorkSettings src) (mc >>= confWorkReportConf)
  reportSettingFreeSettings <- combineToFreeReportSettings (reportSettingFreeSettings src) (mc >>= confFreeReportConf)
  pure $ ReportSettings {..}

combineToWaitingReportSettings :: WaitingReportSettings -> Maybe WaitingReportConfiguration -> IO WaitingReportSettings
combineToWaitingReportSettings wrc mc = do
  let WaitingReportSettings _ = undefined
  pure $
    wrc
      { waitingReportSettingThreshold = fromMaybe defaultWaitingThreshold $ mc >>= waitingReportConfThreshold
      }

combineToStuckReportSettings :: StuckReportSettings -> Maybe StuckReportConfiguration -> IO StuckReportSettings
combineToStuckReportSettings wrc mc = do
  let StuckReportSettings _ = undefined
  pure $
    wrc
      { stuckReportSettingThreshold = fromMaybe defaultStuckThreshold $ mc >>= stuckReportConfThreshold
      }

combineToWorkReportSettings :: WorkReportSettings -> Maybe WorkReportConfiguration -> IO WorkReportSettings
combineToWorkReportSettings wrc mc = do
  let WorkReportSettings _ _ _ _ _ _ = undefined
  pure $
    wrc
      { workReportSettingBaseFilter =
          (mc >>= workReportConfBaseFilter) <|> workReportSettingBaseFilter wrc,
        workReportSettingChecks = fromMaybe (workReportSettingChecks wrc) (mc >>= workReportConfChecks),
        workReportSettingContexts = fromMaybe (workReportSettingContexts wrc) (mc >>= workReportConfContexts),
        workReportSettingTimeProperty = mc >>= workReportConfTimeFilterProperty,
        workReportSettingProjection = fromMaybe defaultProjection (mc >>= workReportConfProjection),
        workReportSettingSorter = mc >>= workReportConfSorter
      }

combineToFreeReportSettings :: FreeReportSettings -> Maybe FreeReportConfiguration -> IO FreeReportSettings
combineToFreeReportSettings wrc mc = do
  let FreeReportSettings _ _ = undefined
  pure $
    wrc
      { freeReportSettingEarliestTimeOfDay = maybe (freeReportSettingEarliestTimeOfDay defaultFreeReportSettings) freeReportConfigurationEarliestTimeOfDay mc,
        freeReportSettingLatestTimeOfDay = maybe (freeReportSettingLatestTimeOfDay defaultFreeReportSettings) freeReportConfigurationLatestTimeOfDay mc
      }

parseFlags :: Parser Flags
parseFlags =
  Flags <$> parseDirectoryFlags

parseHistoricityFlag :: Parser (Maybe AgendaHistoricity)
parseHistoricityFlag =
  optional (flag' HistoricalAgenda (long "historical") <|> flag' FutureAgenda (long "future"))

parseHideArchiveFlag :: Parser (Maybe HideArchive)
parseHideArchiveFlag =
  optional
    ( flag' HideArchive (mconcat [long "hide-archived", help "ignore archived files."])
        <|> flag'
          Don'tHideArchive
          (mconcat [short 'a', long "show-archived", help "Don't ignore archived files."])
    )

parseContextNameArg :: Parser (Maybe ContextName)
parseContextNameArg =
  optional $ argument (ContextName <$> str) (mconcat [metavar "CONTEXT", help "The context that you are in"])

parseTimeFilterArg :: Parser (Maybe Time)
parseTimeFilterArg =
  optional $
    argument
      (eitherReader (parseTime . T.pack))
      (mconcat [metavar "TIME_FILTER", help "A filter to filter by time"])

parseFilterOptionsRel :: Parser (Maybe EntryFilter)
parseFilterOptionsRel =
  fmap foldFilterAnd . NE.nonEmpty
    <$> many
      ( option
          (eitherReader (left (T.unpack . prettyFilterParseError) . parseEntryFilter . T.pack))
          (mconcat [short 'f', long "filter", metavar "FILTER", help "A filter to filter entries by"])
      )

parseFilterArgsRel :: Parser (Maybe EntryFilter)
parseFilterArgsRel =
  fmap foldFilterAnd . NE.nonEmpty
    <$> many
      ( argument
          (eitherReader (left (T.unpack . prettyFilterParseError) . parseEntryFilter . T.pack))
          (mconcat [metavar "FILTER", help "A filter to filter entries by"])
      )

parseArchivedProjectsDirFlag :: Parser (Maybe FilePath)
parseArchivedProjectsDirFlag =
  optional
    ( strOption
        ( mconcat
            [ metavar "DIRECTORY_PATHPATH",
              help "The archived projects directory to use",
              long "archived-projects-dir",
              completer $ bashCompleter "directory"
            ]
        )
    )

parseProjectFilterArgs :: Parser (Maybe ProjectFilter)
parseProjectFilterArgs =
  fmap foldFilterAnd . NE.nonEmpty
    <$> many
      ( argument
          (eitherReader (left (T.unpack . prettyFilterParseError) . parseProjectFilter . T.pack))
          (mconcat [metavar "FILTER", help "A filter to filter projects by"])
      )

parseFileFilterArgs :: Parser (Maybe (Filter (Path Rel File)))
parseFileFilterArgs =
  fmap foldFilterAnd . NE.nonEmpty
    <$> many
      ( argument
          (eitherReader (left (T.unpack . prettyFilterParseError) . parseProjectFilter . T.pack))
          (mconcat [metavar "FILTER", help "A filter to smos files by"])
      )

parseProjectionArgs :: Parser (Maybe (NonEmpty Projection))
parseProjectionArgs =
  NE.nonEmpty . catMaybes
    <$> many
      ( option
          (Just <$> eitherReader (parseProjection . T.pack))
          ( mconcat
              [ long "add-column",
                long "project",
                metavar "PROJECTION",
                help "A projection to project entries onto fields"
              ]
          )
      )

parseSorterArgs :: Parser (Maybe Sorter)
parseSorterArgs =
  fmap (foldl1 AndThen) . NE.nonEmpty . catMaybes
    <$> many
      ( option
          (Just <$> eitherReader (parseSorter . T.pack))
          (mconcat [long "sort", metavar "SORTER", help "A sorter to sort entries by"])
      )

parseTimeBlock :: Parser (Maybe TimeBlock)
parseTimeBlock =
  optional
    ( asum
        [ flag' DayBlock $ mconcat [long "day-block", help "blocks of one day"],
          flag' WeekBlock $ mconcat [long "week-block", help "blocks of one week"],
          flag' MonthBlock $ mconcat [long "month-block", help "blocks of one month"],
          flag' YearBlock $ mconcat [long "year-block", help "blocks of one year"],
          flag' OneBlock $ mconcat [long "one-block", help "a single block"]
        ]
    )

parsePeriod :: Parser (Maybe Period)
parsePeriod =
  parseBeginEnd
    <|> optional
      ( asum
          [ flag' Yesterday (mconcat [long "yesterday", help "yesterday"]),
            flag' Today (mconcat [long "today", help "today"]),
            flag' Tomorrow (mconcat [long "tomorrow", help "tomorrow"]),
            flag' LastWeek (mconcat [long "last-week", help "last week"]),
            flag' PastWeek (mconcat [long "past-week", help "the past week"]),
            flag' ThisWeek (mconcat [long "this-week", help "this week"]),
            flag' ComingWeek (mconcat [long "coming-week", help "the coming week"]),
            flag' NextWeek (mconcat [long "next-week", help "next week"]),
            flag' LastMonth (mconcat [long "last-month", help "last month"]),
            flag' PastMonth (mconcat [long "past-month", help "the past month"]),
            flag' ThisMonth (mconcat [long "this-month", help "this month"]),
            flag' ComingMonth (mconcat [long "coming-month", help "the coming month"]),
            flag' NextMonth (mconcat [long "next-month", help "next month"]),
            flag' LastYear (mconcat [long "last-year", help "last year"]),
            flag' PastYear (mconcat [long "past-year", help "the past year"]),
            flag' ThisYear (mconcat [long "this-year", help "this year"]),
            flag' ComingYear (mconcat [long "coming-year", help "the coming year"]),
            flag' NextYear (mconcat [long "next-year", help "next year"]),
            flag' AllTime (mconcat [long "all-time", help "all time"])
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
          ( option
              (maybeReader parseBegin)
              (mconcat [long "begin", metavar "DAY", help "start date (inclusive)"])
          )
        <*> optional
          ( option
              (maybeReader parseEnd)
              (mconcat [long "end", metavar "DAY", help "end time (inclusive)"])
          )
    parseBegin :: String -> Maybe Day
    parseBegin s = parseLocalDay s
    parseEnd :: String -> Maybe Day
    parseEnd s = addDays 1 <$> parseLocalDay s
    parseLocalDay :: String -> Maybe Day
    parseLocalDay = parseTimeM True defaultTimeLocale "%F"

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") prefixedEnvironmentParser

prefixedEnvironmentParser :: Env.Parser Env.Error Environment
prefixedEnvironmentParser = Env.prefixed "SMOS_" environmentParser

environmentParser :: Env.Parser Env.Error Environment
environmentParser = Environment <$> directoryEnvironmentParser
