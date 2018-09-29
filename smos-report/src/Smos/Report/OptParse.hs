{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Smos.Report.OptParse
    ( module Smos.Report.OptParse
    , module Smos.Report.OptParse.Types
    ) where

import Data.Configurator
import Import hiding (lookup)
import Options.Applicative
import Smos.Report.OptParse.Types
import System.Environment

getInstructions :: IO Instructions
getInstructions = do
    (cmd, flags) <- getArguments
    config <- getConfig flags
    (getDispatch cmd, ) <$> getSettings flags config

getConfig :: Flags -> IO Configuration
getConfig Flags {..} = do
    configPath <- fromMaybe defaultConfigFile $ resolveFile' <$> flagConfigFile
    config <- load [Optional $ toFilePath configPath]
    Configuration <$> lookup config "workDir" <*> lookup config "shouldPrint"
  where
    defaultConfigFile = (</>) <$> getHomeDir <*> parseRelFile ".wfrc"

getSettings :: Flags -> Configuration -> IO Settings
getSettings Flags {..} Configuration {..} = do
    setWorkDir <-
        case flagWorkDir <|> configWorkDir of
            Nothing -> error "No work directory was provided."
            Just dir -> parseAbsDir dir
    let setShouldPrint =
            fromMaybe defaultShouldPrint $ flagShouldPrint <|> configShouldPrint
    pure Settings {..}
  where
    defaultShouldPrint = PrintWarning

getDispatch :: Command -> Dispatch
getDispatch CommandWaiting = DispatchWaiting
getDispatch CommandNext = DispatchNext

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
        [command "waiting" parseCommandWaiting, command "next" parseCommandNext]

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
