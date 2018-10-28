{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Archive.OptParse
    ( getSettings
    ) where

import System.Environment

import Path.IO

import Options.Applicative

import Smos.Archive.OptParse.Types

getSettings :: IO Settings
getSettings = do
    flags <- getFlags
    config <- getConfig flags
    deriveSettings flags config

getConfig :: Flags -> IO Configuration
getConfig Flags {..} = pure Configuration

deriveSettings :: Flags -> Configuration -> IO Settings
deriveSettings Flags {..} Configuration = Settings <$> resolveFile' flagFile

getFlags :: IO Flags
getFlags = do
    args <- getArgs
    let result = runArgumentsParser args
    handleParseResult result

runArgumentsParser :: [String] -> ParserResult Flags
runArgumentsParser = execParserPure prefs_ flagsParser
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

flagsParser :: ParserInfo Flags
flagsParser = info (helper <*> parseFlags) help_
  where
    help_ = fullDesc <> progDesc description
    description = "smos-report"

parseFlags :: Parser Flags
parseFlags =
    Flags <$>
    strArgument (mconcat [help "The file to archive", metavar "FILEPATH"])
