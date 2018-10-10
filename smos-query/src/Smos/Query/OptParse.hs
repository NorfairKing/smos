{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.OptParse where

import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import Path.IO

import System.Environment

import Options.Applicative

import Smos.Data
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
getDispatch CommandWaiting = pure DispatchWaiting
getDispatch CommandNext = pure DispatchNext
getDispatch (CommandClock ClockFlags {..}) = do
    mf <- forM clockFlagFile resolveFile'
    pure $
        DispatchClock
            ClockSettings
                { clockSetFile = mf
                , clockSetPeriod = fromMaybe AllTime clockFlagPeriodFlags
                , clockSetResolution =
                      fromMaybe MinutesResolution clockFlagResolutionFlags
                , clockSetBlock = fromMaybe OneBlock clockFlagBlockFlags
                , clockSetTags = clockFlagTags
                }
getDispatch (CommandAgenda AgendaFlags {..}) =
    pure $
    DispatchAgenda
        AgendaSettings
            { agendaSetHistoricity =
                  fromMaybe HistoricalAgenda agendaFlagHistoricity
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
    description = "smos-report"

parseArgs :: Parser Arguments
parseArgs = (,) <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand =
    hsubparser $
    mconcat
        [ command "waiting" parseCommandWaiting
        , command "next" parseCommandNext
        , command "clock" parseCommandClock
        , command "agenda" parseCommandAgenda
        ]

parseCommandWaiting :: ParserInfo Command
parseCommandWaiting = info parser modifier
  where
    modifier = fullDesc <> progDesc "Print the \"waiting\" tasks"
    parser = pure CommandWaiting

parseCommandNext :: ParserInfo Command
parseCommandNext = info parser modifier
  where
    modifier =
        fullDesc <>
        progDesc "Print the next actions and warn if a file does not have one."
    parser = pure CommandNext

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
         (Just <$>
          (flag' Today (long "today") <|> flag' ThisWeek (long "this-week") <|>
           flag' AllTime (long "all-time")) <|>
          pure Nothing) <*>
         (Just <$>
          (flag' SecondsResolution (long "seconds-resolution") <|>
           flag' MinutesResolution (long "minutes-resolution") <|>
           flag' HoursResolution (long "hours-resolution")) <|>
          pure Nothing) <*>
         (Just <$>
          (flag' DailyBlock (long "daily-block") <|>
           flag' OneBlock (long "one-block")) <|>
          pure Nothing) <*>
         many
             (option
                  (eitherReader (parseTag . T.pack))
                  (mconcat [long "tag", help "filter by this tag"])))

parseCommandAgenda :: ParserInfo Command
parseCommandAgenda = info parser modifier
  where
    modifier = fullDesc <> progDesc "Print the agenda"
    parser =
        CommandAgenda <$>
        (AgendaFlags <$>
         (Just <$>
          (flag' HistoricalAgenda (long "historical") <|>
           flag' FutureAgenda (long "future")) <|>
          pure Nothing))

parseFlags :: Parser Flags
parseFlags = pure Flags
