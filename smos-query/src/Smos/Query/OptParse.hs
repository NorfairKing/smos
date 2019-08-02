{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.OptParse where

import Control.Monad
import Data.Functor
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Path.IO
import Text.Read (readMaybe)

import qualified System.Environment as System

import Options.Applicative

import qualified Smos.Report.OptParse as Report

import Smos.Report.Filter
import Smos.Report.Period
import Smos.Report.Projection
import Smos.Report.Sorter
import Smos.Report.TimeBlock

import Smos.Query.Config
import Smos.Query.OptParse.Types

getInstructions :: SmosQueryConfig -> IO Instructions
getInstructions sqc = do
  Arguments cmd flags <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  Instructions <$> getDispatch cmd <*> getSettings sqc flags env config

getDispatch :: Command -> IO Dispatch
getDispatch c =
  case c of
    CommandEntry EntryFlags {..} ->
      pure $
      DispatchEntry
        EntrySettings
          { entrySetFilter = entryFlagFilter
          , entrySetProjection = entryFlagProjection
          , entrySetSorter = entryFlagSorter
          }
    CommandWork WorkFlags {..} -> pure $ DispatchWork WorkSettings {workSetFilter = workFlagFilter}
    CommandWaiting WaitingFlags {..} ->
      pure $ DispatchWaiting WaitingSettings {waitingSetFilter = waitingFlagFilter}
    CommandNext NextFlags {..} -> pure $ DispatchNext NextSettings {nextSetFilter = nextFlagFilter}
    CommandClock ClockFlags {..} -> do
      mf <- forM clockFlagFile resolveFile'
      pure $
        DispatchClock
          ClockSettings
            { clockSetFile = mf
            , clockSetFilter = clockFlagFilter
            , clockSetPeriod = fromMaybe AllTime clockFlagPeriodFlags
            , clockSetResolution = fromMaybe MinutesResolution clockFlagResolutionFlags
            , clockSetBlock = fromMaybe OneBlock clockFlagBlockFlags
            , clockSetOutputFormat = fromMaybe OutputPretty clockFlagOutputFormat
            , clockSetReportStyle = fromMaybe ClockForest clockFlagReportStyle
            }
    CommandAgenda AgendaFlags {..} ->
      pure $
      DispatchAgenda
        AgendaSettings
          { agendaSetFilter = agendaFlagFilter
          , agendaSetHistoricity = fromMaybe HistoricalAgenda agendaFlagHistoricity
          , agendaSetBlock = fromMaybe OneBlock agendaFlagBlock
          }
    CommandProjects -> pure DispatchProjects
    CommandLog LogFlags {..} ->
      pure $
      DispatchLog
        LogSettings
          { logSetFilter = logFlagFilter
          , logSetPeriod = fromMaybe AllTime logFlagPeriodFlags
          , logSetBlock = fromMaybe OneBlock logFlagBlockFlags
          }
    CommandStats StatsFlags {..} ->
      pure $ DispatchStats StatsSettings {statsSetPeriod = fromMaybe AllTime statsFlagPeriodFlags}
    CommandTags TagsFlags {..} -> pure $ DispatchTags TagsSettings {tagsSetFilter = tagsFlagFilter}

getSettings :: SmosQueryConfig -> Flags -> Environment -> Maybe Configuration -> IO SmosQueryConfig
getSettings SmosQueryConfig {..} Flags {..} Environment {..} mc = do
  src <-
    Report.combineToConfig
      smosQueryConfigReportConfig
      flagReportFlags
      envReportEnvironment
      (confReportConf <$> mc)
  pure $
    SmosQueryConfig
      { smosQueryConfigReportConfig = src
      , smosQueryConfigHideArchive =
          fromMaybe smosQueryConfigHideArchive $
          flagHideArchive <|> envHideArchive <|> (mc >>= confHideArchive)
      }

getEnvironment :: IO Environment
getEnvironment = do
  env <- System.getEnvironment
  let getSmosEnv :: String -> Maybe String
      getSmosEnv key = ("SMOS_" ++ key) `lookup` env
      readSmosEnv :: Read a => String -> Maybe a
      readSmosEnv key = getSmosEnv key >>= readMaybe
  envReportEnvironment <- Report.getEnvironment
  let envHideArchive =
        readSmosEnv "IGNORE_ARCHIVE" <&> \b ->
          case b of
            True -> Don'tHideArchive
            False -> HideArchive
  pure Environment {..}

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  Report.getConfiguration flagReportFlags envReportEnvironment

getArguments :: IO Arguments
getArguments = do
  args <- System.getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = execParserPure prefs_ argParser
  where
    prefs_ =
      ParserPrefs
        { prefMultiSuffix = ""
        , prefDisambiguate = True
        , prefShowHelpOnError = True
        , prefShowHelpOnEmpty = True
        , prefBacktrack = True
        , prefColumns = 80
        }

argParser :: ParserInfo Arguments
argParser = info (helper <*> parseArgs) help_
  where
    help_ = fullDesc <> progDesc description
    description = "smos-query"

parseArgs :: Parser Arguments
parseArgs = Arguments <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand =
  hsubparser $
  mconcat
    [ command "entry" parseCommandEntry
    , command "work" parseCommandWork
    , command "waiting" parseCommandWaiting
    , command "next" parseCommandNext
    , command "clock" parseCommandClock
    , command "agenda" parseCommandAgenda
    , command "projects" parseCommandProjects
    , command "log" parseCommandLog
    , command "stats" parseCommandStats
    , command "tags" parseCommandTags
    ]

parseCommandEntry :: ParserInfo Command
parseCommandEntry = info parser modifier
  where
    modifier = fullDesc <> progDesc "Select entries based on a given filter"
    parser =
      CommandEntry <$> (EntryFlags <$> parseFilterArgs <*> parseProjectionArgs <*> parseSorterArgs)

parseCommandWork :: ParserInfo Command
parseCommandWork = info parser modifier
  where
    modifier = fullDesc <> progDesc "Show the work overview"
    parser = CommandWork <$> (WorkFlags <$> parseFilterArgs)

parseCommandWaiting :: ParserInfo Command
parseCommandWaiting = info parser modifier
  where
    modifier = fullDesc <> progDesc "Print the \"waiting\" tasks"
    parser = CommandWaiting <$> (WaitingFlags <$> parseFilterArgs)

parseCommandNext :: ParserInfo Command
parseCommandNext = info parser modifier
  where
    modifier = fullDesc <> progDesc "Print the next actions and warn if a file does not have one."
    parser = CommandNext <$> (NextFlags <$> parseFilterArgs)

parseCommandClock :: ParserInfo Command
parseCommandClock = info parser modifier
  where
    modifier = fullDesc <> progDesc "Print the clock table"
    parser =
      CommandClock <$>
      (ClockFlags <$>
       (option
          (Just <$> str)
          (mconcat [long "file", help "A single file to gather clock info from", value Nothing])) <*>
       parseFilterArgs <*>
       parsePeriod <*>
       (Just <$>
        (flag' SecondsResolution (long "seconds-resolution") <|>
         flag' MinutesResolution (long "minutes-resolution") <|>
         flag' HoursResolution (long "hours-resolution")) <|>
        pure Nothing) <*>
       parseTimeBlock <*>
       parseOutputFormat <*>
       parseClockReportStyle)

parseClockReportStyle :: Parser (Maybe ClockReportStyle)
parseClockReportStyle =
  (Just <$> (flag' ClockForest (long "forest") <|> flag' ClockFlat (long "flat")) <|> pure Nothing)

parseCommandAgenda :: ParserInfo Command
parseCommandAgenda = info parser modifier
  where
    modifier = fullDesc <> progDesc "Print the agenda"
    parser =
      CommandAgenda <$>
      (AgendaFlags <$> parseFilterArgs <*>
       (Just <$> (flag' HistoricalAgenda (long "historical") <|> flag' FutureAgenda (long "future")) <|>
        pure Nothing) <*>
       parseTimeBlock)

parseCommandProjects :: ParserInfo Command
parseCommandProjects = info parser modifier
  where
    modifier = fullDesc <> progDesc "Print the projects overview"
    parser = pure CommandProjects

parseCommandLog :: ParserInfo Command
parseCommandLog = info parser modifier
  where
    modifier = fullDesc <> progDesc "Print a log of what has happened."
    parser = CommandLog <$> (LogFlags <$> parseFilterArgs <*> parsePeriod <*> parseTimeBlock)

parseCommandStats :: ParserInfo Command
parseCommandStats = info parser modifier
  where
    modifier = fullDesc <> progDesc "Print the stats actions and warn if a file does not have one."
    parser = CommandStats <$> (StatsFlags <$> parsePeriod)

parseCommandTags :: ParserInfo Command
parseCommandTags = info parser modifier
  where
    modifier = fullDesc <> progDesc "Print all the tags that are in use"
    parser = CommandTags <$> (TagsFlags <$> parseFilterArgs)

parseFlags :: Parser Flags
parseFlags = Flags <$> Report.parseFlags <*> parseHideArchiveFlag

parseHideArchiveFlag :: Parser (Maybe HideArchive)
parseHideArchiveFlag =
  (Just <$>
   ((flag' HideArchive (mconcat [long "hide-archived", help "ignore archived files."])) <|>
    (flag'
       Don'tHideArchive
       (mconcat [short 'a', long "show-archived", help "Don't ignore archived files."])))) <|>
  (pure Nothing)

parseFilterArgs :: Parser (Maybe Filter)
parseFilterArgs =
  (fmap foldFilterAnd . NE.nonEmpty) <$>
  many
    (argument
       (maybeReader (parseFilter . T.pack))
       (mconcat
          [ metavar "FILTER"
          , help "A filter to filter entries by"
          , completer $ mkCompleter $ pure . filterCompleter
          ]))

parseFilterArg :: Parser (Maybe Filter)
parseFilterArg =
  argument
    (Just <$> (maybeReader (parseFilter . T.pack)))
    (mconcat
       [ value Nothing
       , metavar "FILTER"
       , help "A filter to filter entries by"
       , completer $ mkCompleter $ pure . filterCompleter
       ])

parseProjectionArgs :: Parser (Maybe Projection)
parseProjectionArgs =
  (fmap (foldl1 AndAlso) . NE.nonEmpty . catMaybes) <$>
  many
    (option
       (Just <$> (maybeReader (parseProjection . T.pack)))
       (mconcat
          [long "project", metavar "PROJECTION", help "A projection to project entries onto fields"]))

parseSorterArgs :: Parser (Maybe Sorter)
parseSorterArgs =
  (fmap (foldl1 AndThen) . NE.nonEmpty . catMaybes) <$>
  many
    (option
       (Just <$> (maybeReader (parseSorter . T.pack)))
       (mconcat [long "sort", metavar "SORTER", help "A sorter to sort entries by"]))

parseTimeBlock :: Parser (Maybe TimeBlock)
parseTimeBlock =
  Just <$>
  (choices
     [ flag' DayBlock $ mconcat [long "day-block", help "blocks of one day"]
     , flag' OneBlock $ mconcat [long "one-block", help "a single block"]
     ]) <|>
  pure Nothing

parsePeriod :: Parser (Maybe Period)
parsePeriod =
  Just <$>
  (parseBeginEnd <|>
   choices
     [ flag' Today (mconcat [long "today", help "today"])
     , flag' ThisWeek (mconcat [long "this-week", help "this week"])
     , flag' LastWeek (mconcat [long "last-week", help "last week"])
     , flag' ThisMonth (mconcat [long "this-month", help "this month"])
     , flag' LastMonth (mconcat [long "last-month", help "last month"])
     , flag' AllTime (mconcat [long "all-time", help "all time"])
     ]) <|>
  pure Nothing
  where
    parseBeginEnd :: Parser Period
    parseBeginEnd =
      BeginEnd <$>
      option
        (maybeReader parseLocalBegin)
        (mconcat [long "begin", metavar "LOCALTIME", help "start time (inclusive)"]) <*>
      option
        (maybeReader parseLocalEnd)
        (mconcat [long "end", metavar "LOCALTIME", help "end tiem (inclusive)"])
    parseLocalBegin :: String -> Maybe LocalTime
    parseLocalBegin s = LocalTime <$> parseLocalDay s <*> pure midnight <|> parseExactly s
    parseLocalEnd :: String -> Maybe LocalTime
    parseLocalEnd s =
      (LocalTime <$> (addDays 1 <$> parseLocalDay s) <*> pure midnight) <|> parseExactly s
    parseExactly :: String -> Maybe LocalTime
    parseExactly s =
      parseTimeM True defaultTimeLocale "%F %R" s <|> parseTimeM True defaultTimeLocale "%F %T" s
    parseLocalDay :: String -> Maybe Day
    parseLocalDay = parseTimeM True defaultTimeLocale "%F"

parseOutputFormat :: Parser (Maybe OutputFormat)
parseOutputFormat =
  Just <$>
  (choices
     [ flag' OutputPretty $ mconcat [long "pretty", help "pretty text"]
     , flag' OutputYaml $ mconcat [long "yaml", help "Yaml"]
     , flag' OutputJSON $ mconcat [long "json", help "single-line JSON"]
     , flag' OutputJSONPretty $ mconcat [long "pretty-json", help "pretty JSON"]
     ]) <|>
  pure Nothing

choices :: [Parser a] -> Parser a
choices [] = empty
choices (a:as) = a <|> choices as
