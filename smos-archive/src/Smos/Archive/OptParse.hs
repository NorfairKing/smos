{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Archive.OptParse
  ( getSettings
  ) where

import Control.Monad
import Path.IO

import System.Environment

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
deriveSettings Flags {..} Configuration = do
  setFile <- resolveFile' flagFile
  setWorkflowDir <- forM flagWorkflowDir resolveDir'
  setArchiveDir <- forM flagArchiveDir resolveDir'
  pure Settings {..}

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
  Flags <$> strArgument (mconcat [help "The file to archive", metavar "FILEPATH"]) <*>
  option
    (Just <$> str)
    (mconcat [long "workflow-dir", help "The workflow directory", metavar "FILEPATH", value Nothing]) <*>
  option
    (Just <$> str)
    (mconcat [long "archive-dir", help "The archive directory", metavar "FILEPATH", value Nothing])
