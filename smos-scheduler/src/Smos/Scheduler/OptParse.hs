{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Scheduler.OptParse
  ( module Smos.Scheduler.OptParse,
    module Smos.Scheduler.OptParse.Types,
  )
where

import Data.Maybe
import qualified Env
import Options.Applicative
import Path
import Path.IO
import qualified Smos.Report.Config as Report
import qualified Smos.Report.OptParse as Report
import Smos.Scheduler.OptParse.Types
import qualified System.Environment as System

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  env <- getEnvironment
  config <- getConfiguration flags env
  deriveSettings (Report.flagWithRestFlags flags) (Report.envWithRestEnv env) config

deriveSettings :: Flags -> Environment -> Maybe Configuration -> IO Settings
deriveSettings Flags {..} Environment {..} mc = do
  setDirectorySettings <-
    Report.combineToDirectoryConfig
      Report.defaultDirectoryConfig
      flagDirectoryFlags
      envDirectoryEnvironment
      (confDirectoryConfiguration <$> mc)
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

getConfiguration :: Report.FlagsWithConfigFile Flags -> Report.EnvWithConfigFile Environment -> IO (Maybe Configuration)
getConfiguration = Report.getConfiguration

getEnvironment :: IO (Report.EnvWithConfigFile Environment)
getEnvironment = Env.parse (Env.header "Environment") prefixedEnvironmentParser

prefixedEnvironmentParser :: Env.Parser Env.Error (Report.EnvWithConfigFile Environment)
prefixedEnvironmentParser = Env.prefixed "SMOS_" environmentParser

environmentParser :: Env.Parser Env.Error (Report.EnvWithConfigFile Environment)
environmentParser =
  Report.envWithConfigFileParser $
    Environment
      <$> Report.directoryEnvironmentParser
      <*> Env.var (fmap Just . Env.str) "STATE_FILE" (mE <> Env.help "The path to the file in which to store the scheduler state")
  where
    mE = Env.def Nothing <> Env.keep

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
flagsParser = info (helper <*> parseFlags) help_
  where
    help_ = fullDesc <> progDesc description
    description = "smos-scheduler"

parseFlags :: Parser (Report.FlagsWithConfigFile Flags)
parseFlags =
  Report.parseFlagsWithConfigFile $
    Flags <$> Report.parseDirectoryFlags
      <*> option (Just <$> str) (mconcat [long "state-file", help "The state file to use", value Nothing])
