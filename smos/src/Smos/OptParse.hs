{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.OptParse
    ( getInstructions
    , Instructions(..)
    ) where

import Import

import qualified Data.Text.IO as T

import System.Environment (getArgs, getEnvironment)
import System.Exit (die)

import Data.Aeson as JSON (eitherDecodeFileStrict)
import Data.Yaml as Yaml (decodeFileEither, prettyPrintParseException)

import Dhall

import Options.Applicative


import Smos.OptParse.Bare
import Smos.OptParse.Types
import Smos.Report.Config
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
    src <- Report.combineToConfig configReportConfig flagReportFlags envReportEnv (configReportConfig <$> mc)
    let sc' =
                      sc {configReportConfig = src}
    pure $ Instructions p sc'

getConfiguration :: Arguments -> Environment -> IO (Maybe Configuration)
getConfiguration (Arguments _ Flags {..}) Environment {..} = do
    mConfigFile <-
        case flagConfigFile <|> envConfigFile of
            Nothing ->
                msum <$>
                Import.sequence
                    [ defaultDhallConfigFile
                    , defaultYamlConfigFile
                    , defaultJSONConfigFile
                    ]
            Just fp -> Just <$> resolveFile' fp
    forM mConfigFile $ \configFile ->
        case fileExtension configFile of
            ".dhall" -> do
                contents <- T.readFile $ fromAbsFile configFile
                detailed
                    (input
                         configurationType
                         (configurationDefaults <> "//" <> contents))
            ".json" -> do
                errOrConfig <-
                    JSON.eitherDecodeFileStrict $ fromAbsFile configFile
                case errOrConfig of
                    Left err -> die err
                    Right config -> pure config
            -- As Yaml
            _ -> do
                errOrConfig <- decodeFileEither $ fromAbsFile configFile
                case errOrConfig of
                    Left err -> die $ prettyPrintParseException err
                    Right config -> pure config

defaultDhallConfigFile :: IO (Maybe (Path Abs File))
defaultDhallConfigFile = do
    home <- getHomeDir
    p <- resolveFile home ".smos.dhall"
    e <- doesFileExist p
    pure $
        if e
            then Just p
            else Nothing

defaultJSONConfigFile :: IO (Maybe (Path Abs File))
defaultJSONConfigFile = do
    home <- getHomeDir
    p <- resolveFile home ".smos.json"
    e <- doesFileExist p
    pure $
        if e
            then Just p
            else Nothing

defaultYamlConfigFile :: IO (Maybe (Path Abs File))
defaultYamlConfigFile = do
    home <- getHomeDir
    p <- resolveFile home ".smos.yaml"
    e <- doesFileExist p
    pure $
        if e
            then Just p
            else Nothing

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
parseFlags = Flags <$> parseConfigFileFlag <*> parseWorkflowDirFlag

parseConfigFileFlag :: Parser (Maybe FilePath)
parseConfigFileFlag =
    option
        (Just <$> str)
        (mconcat
             [ metavar "FILEPATH"
             , help "The configuration file to use"
             , value Nothing
             ])
