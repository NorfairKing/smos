{-# LANGUAGE RecordWildCards #-}

module Smos.OptParse
    ( getInstructions
    , Instructions(..)
    ) where

import Import

import System.Environment (getArgs, getEnvironment)

import Options.Applicative

import Smos.OptParse.Bare
import Smos.OptParse.Types
import Smos.Types

getInstructions :: SmosConfig -> IO Instructions
getInstructions conf = do
    args <- getArguments
    env <- getEnv
    config <- getConfiguration args
    combineToInstructions conf args env config

combineToInstructions ::
       SmosConfig
    -> Arguments
    -> Environment
    -> Configuration
    -> IO Instructions
combineToInstructions sc@SmosConfig {..} (Arguments fp Flags) Environment Configuration = do
    p <- resolveFile' fp
    pure $ Instructions p sc

getConfiguration :: Arguments -> IO Configuration
getConfiguration _ = pure Configuration

getEnv :: IO Environment
getEnv = do
    _ <- getEnvironment
    pure Environment

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
parseFlags = pure Flags
