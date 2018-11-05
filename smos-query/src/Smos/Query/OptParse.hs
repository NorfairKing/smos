{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.OptParse where

import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import Path.IO

import System.Environment

import Options.Applicative

import Smos.Report.Period
import Smos.Report.Query
import Smos.Report.TimeBlock

import Smos.Query.OptParse.Types

getInstructions :: IO Instructions
getInstructions = do
    (cmd, flags) <- getArguments
    config <- getConfig flags
    (,) <$> getDispatch cmd <*> getSettings flags config

getConfig :: Flags -> IO Configuration
getConfig Flags = pure Configuration

getSettings :: Flags -> Configuration -> IO Settings
getSettings Flags Configuration = pure Settings

getDispatch :: Command -> IO Dispatch
getDispatch c =
    case c of
        CommandEntry EntryFlags {..} ->
            pure $
            DispatchEntry EntrySettings {entrySetFilter = entryFlagFilter}
        CommandWaiting WaitingFlags {..} ->
            pure $
            DispatchWaiting
                WaitingSettings {waitingSetFilter = waitingFlagFilter}
        CommandNext NextFlags {..} ->
            pure $ DispatchNext NextSettings {nextSetFilter = nextFlagFilter}
        CommandClock ClockFlags {..} -> do
            mf <- forM clockFlagFile resolveFile'
            pure $
                DispatchClock
                    ClockSettings
                        { clockSetFile = mf
                        , clockSetFilter = clockFlagFilter
                        , clockSetPeriod =
                              fromMaybe AllTime clockFlagPeriodFlags
                        , clockSetResolution =
                              fromMaybe
                                  MinutesResolution
                                  clockFlagResolutionFlags
                        , clockSetBlock = fromMaybe OneBlock clockFlagBlockFlags
                        }
        CommandAgenda AgendaFlags {..} ->
            pure $
            DispatchAgenda
                AgendaSettings
                    { agendaSetFilter = agendaFlagFilter
                    , agendaSetHistoricity =
                          fromMaybe HistoricalAgenda agendaFlagHistoricity
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
            pure $
            DispatchStats
                StatsSettings
                    { statsSetFilter = statsFlagFilter
                    , statsSetPeriod = fromMaybe AllTime statsFlagPeriodFlags
                    }

getArguments :: IO Arguments
getArguments = do
    args <- getArgs
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
parseArgs = (,) <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand =
    hsubparser $
    mconcat
        [ command "entry" parseCommandEntry
        , command "waiting" parseCommandWaiting
        , command "next" parseCommandNext
        , command "clock" parseCommandClock
        , command "agenda" parseCommandAgenda
        , command "projects" parseCommandProjects
        , command "log" parseCommandLog
        , command "stats" parseCommandStats
        ]

parseCommandEntry :: ParserInfo Command
parseCommandEntry = info parser modifier
  where
    modifier = fullDesc <> progDesc "Select entries based on a given filter"
    parser = CommandEntry <$> (EntryFlags <$> parseFilterArg)

parseCommandWaiting :: ParserInfo Command
parseCommandWaiting = info parser modifier
  where
    modifier = fullDesc <> progDesc "Print the \"waiting\" tasks"
    parser = CommandWaiting <$> (WaitingFlags <$> parseFilterArg)

parseCommandNext :: ParserInfo Command
parseCommandNext = info parser modifier
  where
    modifier =
        fullDesc <>
        progDesc "Print the next actions and warn if a file does not have one."
    parser = CommandNext <$> (NextFlags <$> parseFilterArg)

parseCommandClock :: ParserInfo Command
parseCommandClock = info parser modifier
  where
    modifier = fullDesc <> progDesc "Print the clock table"
    parser =
        CommandClock <$>
        (ClockFlags <$>
         (option
              (Just <$> str)
              (mconcat
                   [ long "file"
                   , help "A single file to gather clock info from"
                   , value Nothing
                   ])) <*>
         parseFilterArg <*>
         parsePeriod <*>
         (Just <$>
          (flag' SecondsResolution (long "seconds-resolution") <|>
           flag' MinutesResolution (long "minutes-resolution") <|>
           flag' HoursResolution (long "hours-resolution")) <|>
          pure Nothing) <*>
         parseTimeBlock)

parseCommandAgenda :: ParserInfo Command
parseCommandAgenda = info parser modifier
  where
    modifier = fullDesc <> progDesc "Print the agenda"
    parser =
        CommandAgenda <$>
        (AgendaFlags <$> parseFilterArg <*>
         (Just <$>
          (flag' HistoricalAgenda (long "historical") <|>
           flag' FutureAgenda (long "future")) <|>
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
    parser =
        CommandLog <$>
        (LogFlags <$> parseFilterArg <*> parsePeriod <*> parseTimeBlock)

parseCommandStats :: ParserInfo Command
parseCommandStats = info parser modifier
  where
    modifier =
        fullDesc <>
        progDesc "Print the stats actions and warn if a file does not have one."
    parser = CommandStats <$> (StatsFlags <$> parseFilterArg <*> parsePeriod)

parseFlags :: Parser Flags
parseFlags = pure Flags

parseFilterArg :: Parser (Maybe Filter)
parseFilterArg =
    argument
        (Just <$> (maybeReader (parseFilter . T.pack)))
        (mconcat [value Nothing, help "A filter to filter entries by"])

parseTimeBlock :: Parser (Maybe TimeBlock)
parseTimeBlock =
    Just <$>
    (flag' DayBlock (long "day-block") <|> flag' OneBlock (long "one-block")) <|>
    pure Nothing

parsePeriod :: Parser (Maybe Period)
parsePeriod =
    Just <$>
    (flag' Today (long "today") <|> flag' ThisWeek (long "this-week") <|>
     flag' LastWeek (long "last-week") <|>
     flag' AllTime (long "all-time")) <|>
    pure Nothing
