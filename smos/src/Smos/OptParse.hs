{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.OptParse
    ( getInstructions
    , Instructions(..)
    ) where

import Import

import System.Environment (getArgs, getEnvironment)

import Options.Applicative

import Smos.OptParse.Bare
import Smos.OptParse.Types
import qualified Smos.Report.OptParse as Report
import Smos.Types

getInstructions :: SmosConfig -> IO Instructions
getInstructions conf = do
    args <- getArguments
    env <- getEnv
    config <- getConfiguration args env
    combineToInstructions conf args env config

combineToInstructions ::
       SmosConfig
    -> Arguments
    -> Environment
    -> Maybe Configuration
    -> IO Instructions
combineToInstructions sc@SmosConfig {..} (Arguments fp Flags {..}) Environment {..} mc = do
    p <- resolveFile' fp
    src <-
        Report.combineToConfig
            configReportConfig
            flagReportFlags
            envReportEnv
            (confReportConf <$> mc)
    let sc' = sc {configReportConfig = src}
    pure $ Instructions p sc'

getConfiguration :: Arguments -> Environment -> IO (Maybe Configuration)
getConfiguration (Arguments _ Flags {..}) Environment {..} =
    Report.getConfigurationWith
        [flagConfigFile, envConfigFile]
        configurationDefaults
        configurationType

getEnv :: IO Environment
getEnv = do
    env <- getEnvironment
    reportEnv <- Report.getEnv
    let getSmosEnv :: String -> Maybe String
        getSmosEnv key = ("SMOS_" ++ key) `lookup` env
    pure
        Environment
        { envConfigFile =
              getSmosEnv "CONFIGURATION_FILE" <|> getSmosEnv "CONFIG_FILE"
        , envReportEnv = reportEnv
        }

getArguments :: IO Arguments
getArguments = runArgumentsParser <$> getArgs >>= handleParseResult

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
    description = "Smos editor"

parseArgs :: Parser Arguments
parseArgs = Arguments <$> editParser <*> parseFlags

parseFlags :: Parser Flags
parseFlags = Flags <$> parseConfigFileFlag <*> Report.parseFlags

parseConfigFileFlag :: Parser (Maybe FilePath)
parseConfigFileFlag =
    option
        (Just <$> str)
        (mconcat
             [ metavar "FILEPATH"
             , help "The configuration file to use"
             , value Nothing
             ])
