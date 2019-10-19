{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Scheduler.OptParse
  ( getSettings
  ) where

import qualified Data.Text as T
import Path

import Control.Monad

import qualified System.Environment as System
import System.Exit

import Options.Applicative

import Smos.Data

import qualified Smos.Report.Config as Report
import qualified Smos.Report.OptParse as Report

import Smos.Scheduler.OptParse.Types

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  env <- getEnvironment
  config <- getConfig flags env
  deriveSettings flags env config

getConfig :: Flags -> Environment -> IO (Maybe Configuration)
getConfig Flags {..} Environment {..} =
  fmap Configuration <$> Report.getConfiguration flagReportFlags envReportEnvironment

deriveSettings :: Flags -> Environment -> Maybe Configuration -> IO Settings
deriveSettings Flags {..} Environment {..} mc = do
  setReportSettings <-
    Report.combineToConfig
      Report.defaultReportConfig
      flagReportFlags
      envReportEnvironment
      (confReportConfiguration <$> mc)
  pure Settings {..}

getFlags :: IO Flags
getFlags = do
  args <- System.getArgs
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
parseFlags = Flags <$> Report.parseFlags

getEnvironment :: IO Environment
getEnvironment = Environment <$> Report.getEnvironment
