{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Scheduler.OptParse
  ( getSettings,
  )
where

import Data.Maybe
import Options.Applicative
import Path
import Path.IO
import qualified Smos.Report.Config as Report
import qualified Smos.Report.OptParse as Report
import qualified Smos.Report.OptParse.Types as Report
import Smos.Scheduler.OptParse.Types
import qualified System.Environment as System
import YamlParse.Applicative (confDesc)

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
getEnvironment = Report.getEnvWithConfigFile $ do
  envDirectoryEnvironment <- Report.getDirectoryEnvironment
  env <- System.getEnvironment
  let getEnv :: String -> Maybe String
      getEnv key = ("SMOS_SCHEDULER" ++ key) `lookup` env
  -- readEnv :: Read a => String -> Maybe a
  -- readEnv key = getEnv key >>= readMaybe
  let envStateFile = getEnv "STATE_FILE"
  pure Environment {..}

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
    help_ = fullDesc <> progDesc description <> confDesc @Configuration
    description = "smos-scheduler"

parseFlags :: Parser (Report.FlagsWithConfigFile Flags)
parseFlags =
  Report.parseFlagsWithConfigFile $
    Flags <$> Report.parseDirectoryFlags
      <*> option (Just <$> str) (mconcat [long "state-file", help "The state file to use", value Nothing])
