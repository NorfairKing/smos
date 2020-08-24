{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
import Smos.Version
import qualified System.Environment as System

getInstructions :: IO Instructions
getInstructions = do
  (Arguments cmd flags) <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions cmd (Report.flagWithRestFlags flags) (Report.envWithRestEnv env) config

combineToInstructions :: Command -> Flags -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions cmd Flags {..} Environment {..} mc = do
  let d = case cmd of
        CommandCheck -> DispatchCheck
        CommandSchedule -> DispatchSchedule
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
  pure (Instructions d Settings {..})
  where
    cM :: (SchedulerConfiguration -> Maybe a) -> Maybe a
    cM func = mc >>= confSchedulerConfiguration >>= func

defaultStateFile :: IO (Path Abs File)
defaultStateFile = do
  xdg <- getXdgDir XdgData (Just [reldir|smos|])
  resolveFile xdg "scheduler-state.yaml"

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

getArguments :: IO Arguments
getArguments = do
  args <- System.getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = execParserPure prefs_ argumentsParser
  where
    prefs_ =
      defaultPrefs
        { prefShowHelpOnError = True,
          prefShowHelpOnEmpty = True
        }

argumentsParser :: ParserInfo Arguments
argumentsParser = info (helper <*> parseArguments) help_
  where
    help_ = fullDesc <> progDesc description
    description = "Smos Scheduler Tool: " <> smosVersion

parseArguments :: Parser Arguments
parseArguments = Arguments <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand =
  hsubparser $
    mconcat
      [ command "check" parseCommandCheck,
        command "schedule" parseCommandSchedule
      ]

parseCommandCheck :: ParserInfo Command
parseCommandCheck = info parser modifier
  where
    modifier = fullDesc <> progDesc "Check that all schedules are sensible"
    parser = pure CommandCheck

parseCommandSchedule :: ParserInfo Command
parseCommandSchedule = info parser modifier
  where
    modifier = fullDesc <> progDesc "Run the schedules"
    parser = pure CommandSchedule

parseFlags :: Parser (Report.FlagsWithConfigFile Flags)
parseFlags =
  Report.parseFlagsWithConfigFile $
    Flags <$> Report.parseDirectoryFlags
      <*> option
        (Just <$> str)
        ( mconcat
            [ long "state-file",
              help "The state file to use",
              value Nothing,
              metavar "FILEPATH",
              completer $ bashCompleter "file"
            ]
        )
