{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.OptParse
    ( module Smos.Report.OptParse
    , module Smos.Report.OptParse.Types
    ) where

import Data.Maybe

import System.Environment

import Data.Configurator as Configurator
import Path
import Path.IO

import Options.Applicative

import Smos.Report.OptParse.Types

getInstructions :: IO Instructions
getInstructions = do
    (cmd, flags) <- getArguments
    config <- getConfig flags
    (,) (getDispatch cmd) <$> getSettings flags config

getConfig :: Flags -> IO Configuration
getConfig Flags {..} = do
    configPath <- fromMaybe defaultConfigFile $ resolveFile' <$> flagConfigFile
    config <- load [Optional $ toFilePath configPath]
    Configuration <$> Configurator.lookup config "workDir" <*>
        Configurator.lookup config "shouldPrint"
  where
    defaultConfigFile = (</>) <$> getHomeDir <*> parseRelFile ".smosrc"

getSettings :: Flags -> Configuration -> IO Settings
getSettings Flags {..} Configuration {..} = do
    setWorkDir <-
        case flagWorkDir <|> configWorkDir of
            Nothing -> error "No work directory was provided."
            Just dir -> resolveDir' dir
    let setShouldPrint =
            fromMaybe defaultShouldPrint $ flagShouldPrint <|> configShouldPrint
    pure Settings {..}
  where
    defaultShouldPrint = PrintWarning

getDispatch :: Command -> Dispatch
getDispatch CommandWaiting = DispatchWaiting
getDispatch CommandNext = DispatchNext
getDispatch (CommandClock ClockFlags {..}) =
    DispatchClock
        ClockSettings
            { clockSetPeriod = fromMaybe AllTime clockFlagPeriodFlags
            , clockSetResolution = fromMaybe MinutesResolution clockFlagResolutionFlags
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
         (Just <$>
          (flag' Today (long "today") <|> flag' ThisWeek (long "this-week") <|>
           flag' AllTime (long "all-time")) <|>
          pure Nothing) <*>
         (Just <$>
          (flag' SecondsResolution (long "seconds-resolution") <|>
           flag' MinutesResolution (long "minutes-resolution") <|>
           flag' HoursResolution (long "hours-resolution")) <|>
          pure Nothing))

parseFlags :: Parser Flags
parseFlags = Flags <$> configFileParser <*> workDirParser <*> shouldPrintParser

workDirParser :: Parser (Maybe FilePath)
workDirParser =
    option
        (Just <$> str)
        (mconcat
             [ long "work-dir"
             , help "Workflow directory"
             , value Nothing
             , metavar "FILEPATH"
             ])

configFileParser :: Parser (Maybe FilePath)
configFileParser =
    option
        (Just <$> str)
        (mconcat
             [ long "config-file"
             , help "Configuration file"
             , value Nothing
             , metavar "FILEPATH"
             ])

shouldPrintParser :: Parser (Maybe ShouldPrint)
shouldPrintParser =
    option
        (Just <$> maybeReader parseShouldPrint)
        (mconcat
             [ long "should-print"
             , help
                   "This describes whether error messages should be handled as errors (\"error\"), warnings (\"warning\") or ignored (\"nothing\")."
             , value Nothing
             , metavar "shouldPrint"
             ])
