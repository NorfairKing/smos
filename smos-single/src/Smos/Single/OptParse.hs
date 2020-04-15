{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Single.OptParse
  ( getSettings
  ) where

import Control.Monad
import qualified Data.Text as T
import Options.Applicative
import Path
import Smos.Data
import qualified Smos.Report.Config as Report
import qualified Smos.Report.OptParse as Report
import Smos.Single.OptParse.Types
import qualified System.Environment as System
import System.Exit

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
  setTask <-
    case parseHeader $ T.pack $ unwords flagTaskPieces of
      Left err -> die $ "Failed to parse header: " <> err
      Right h -> pure h
  setReportSettings <-
    Report.combineToConfig
      Report.defaultReportConfig
      flagReportFlags
      envReportEnvironment
      (confReportConfiguration <$> mc)
  setTaskFile <- forM flagTaskFile parseRelFile
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
    description = "smos-single"

parseFlags :: Parser Flags
parseFlags =
  Flags <$>
  some
    (strArgument
       (mconcat
          [ help
              "The task. Pass any number of arguments and they will be interpreted as the task together."
          , metavar "TASK"
          ])) <*>
  option
    (Just <$> str)
    (mconcat [long "file", help "The file to put the task in", metavar "FILEPATH", value Nothing]) <*>
  Report.parseFlags

getEnvironment :: IO Environment
getEnvironment = Environment <$> Report.getEnvironment
