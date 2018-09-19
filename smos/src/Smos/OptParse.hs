{-# LANGUAGE RecordWildCards #-}

module Smos.OptParse
    ( getInstructions
    , Instructions(..)
    , Settings(..)
    ) where

import Import

import System.Environment (getArgs)

import Options.Applicative

import Smos.OptParse.Types
import Smos.Types

getInstructions :: SmosConfig -> IO Instructions
getInstructions conf = do
    args <- getArguments
    config <- getConfiguration args
    combineToInstructions conf args config

combineToInstructions ::
       SmosConfig -> Arguments -> Configuration -> IO Instructions
combineToInstructions SmosConfig {..} (Arguments fp Flags) Configuration = do
    p <- resolveFile' fp
    pure $ Instructions p Settings

getConfiguration :: Arguments -> IO Configuration
getConfiguration _ = pure Configuration

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

editParser :: Parser FilePath
editParser =
    strArgument
        (mconcat [metavar "FILE", help "the file to edit",completer $ bashCompleter "file"])

parseFlags :: Parser Flags
parseFlags = pure Flags
