{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Archive.OptParse
  ( getSettings,
  )
where

import Options.Applicative
import Path.IO
import Smos.Archive.OptParse.Types
import qualified Smos.Report.Config as Report
import qualified Smos.Report.OptParse as Report
import qualified Smos.Report.OptParse.Types as Report
import qualified System.Environment as System
import YamlParse.Applicative (confDesc)

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  env <- getEnvironment
  config <- getConfig flags env
  deriveSettings (Report.flagWithRestFlags flags) (Report.envWithRestEnv env) config

getConfig :: Report.FlagsWithConfigFile Flags -> Report.EnvWithConfigFile Environment -> IO (Maybe Configuration)
getConfig f e =
  fmap Configuration <$> Report.getConfiguration f e

deriveSettings :: Flags -> Environment -> Maybe Configuration -> IO Settings
deriveSettings Flags {..} Environment {..} mc = do
  setFile <- resolveFile' flagFile
  setDirectorySettings <-
    Report.combineToDirectoryConfig
      Report.defaultDirectoryConfig
      flagDirectoryFlags
      envDirectoryEnvironment
      (confDirectoryConfiguration <$> mc)
  pure Settings {..}

getFlags :: IO (Report.FlagsWithConfigFile Flags)
getFlags = do
  args <- System.getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult (Report.FlagsWithConfigFile Flags)
runArgumentsParser = execParserPure prefs_ flagsParser
  where
    prefs_ =
      ParserPrefs
        { prefMultiSuffix = "",
          prefDisambiguate = True,
          prefShowHelpOnError = True,
          prefShowHelpOnEmpty = True,
          prefBacktrack = True,
          prefColumns = 80
        }

flagsParser :: ParserInfo (Report.FlagsWithConfigFile Flags)
flagsParser = info (helper <*> Report.parseFlagsWithConfigFile parseFlags) help_
  where
    help_ = fullDesc <> progDesc description <> confDesc @Report.Configuration
    description = "smos-archive"

parseFlags :: Parser Flags
parseFlags =
  Flags <$> strArgument (mconcat [help "The file to archive", metavar "FILEPATH", action "file"])
    <*> Report.parseDirectoryFlags

getEnvironment :: IO (Report.EnvWithConfigFile Environment)
getEnvironment = Report.getEnvWithConfigFile (Environment <$> Report.getDirectoryEnvironment)
