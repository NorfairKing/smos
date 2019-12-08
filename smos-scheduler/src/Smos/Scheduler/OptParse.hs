{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Scheduler.OptParse
  ( getSettings
  ) where

import Data.Maybe
import qualified Data.Text as T

import Path
import Path.IO

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
  config <- getConfiguration flags env
  deriveSettings flags env config

deriveSettings :: Flags -> Environment -> Maybe Configuration -> IO Settings
deriveSettings Flags {..} Environment {..} mc = do
  setReportSettings <-
    Report.combineToConfig
      Report.defaultReportConfig
      flagReportFlags
      envReportEnvironment
      (confReportConfiguration <$> mc)
  setStateFile <-
    case flagStateFile <|> envStateFile <|> cM schedulerConfStateFile of
      Nothing -> defaultStateFile
      Just fp -> resolveFile' fp
  let setSchedule = fromMaybe (Schedule []) $ cM schedulerConfSchedule
  pure Settings {..}
  where
    cM :: (SchedulerConfiguration -> Maybe a) -> Maybe a
    cM func = mc >>= confSchedulerConfiguration >>= func

-- TODO make sure this is in the workflow dir, so that it gets synced.
defaultStateFile :: IO (Path Abs File)
defaultStateFile = do
  home <- getHomeDir
  resolveFile home ".smos/scheduler-state.yaml"

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  Report.getConfiguration flagReportFlags envReportEnvironment

getEnvironment :: IO Environment
getEnvironment = do
  envReportEnvironment <- Report.getEnvironment
  env <- System.getEnvironment
  let getEnv :: String -> Maybe String
      getEnv key = ("SMOS_SCHEDULER" ++ key) `lookup` env
      -- readEnv :: Read a => String -> Maybe a
      -- readEnv key = getEnv key >>= readMaybe
  let envStateFile = getEnv "STATE_FILE"
  pure Environment {..}

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
parseFlags =
  Flags <$> Report.parseFlags <*>
  option (Just <$> str) (mconcat [long "state-file", help "The state file to use", value Nothing])
