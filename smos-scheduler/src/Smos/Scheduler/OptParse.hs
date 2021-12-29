{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Scheduler.OptParse
  ( module Smos.Scheduler.OptParse,
    module Smos.Scheduler.OptParse.Types,
  )
where

import Data.Maybe
import Data.Version
import qualified Env
import Options.Applicative
import Options.Applicative.Help.Pretty as Doc
import Path
import Path.IO
import Paths_smos_scheduler
import Smos.Data
import Smos.Query.OptParse (getColourSettings)
import qualified Smos.Report.Config as Report
import qualified Smos.Report.OptParse as Report
import Smos.Scheduler.OptParse.Types
import qualified System.Environment as System

getInstructions :: IO Instructions
getInstructions = do
  (Arguments cmd flags) <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions cmd (Report.flagWithRestFlags flags) (Report.envWithRestEnv env) config

combineToInstructions :: Command -> Flags -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions cmd Flags {..} Environment {..} mc = do
  d <- case cmd of
    CommandCheck -> pure DispatchCheck
    CommandNext -> pure DispatchNext
    CommandSample fp mdpt -> DispatchSample <$> resolveFile' fp <*> mapM (fmap DestinationPathTemplate . parseRelFile) mdpt
    CommandSchedule -> pure DispatchSchedule
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
  let setColourSettings = getColourSettings (mc >>= confColourConfiguration)
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
      <*> optional (Env.var Env.str "STATE_FILE" (Env.help "The path to the file in which to store the scheduler state"))

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
    help_ = fullDesc <> progDescDoc (Just description)
    description :: Doc
    description =
      Doc.vsep $
        map Doc.text $
          [ "",
            "Smos Scheduler Tool version: " <> showVersion version,
            ""
          ]
            ++ writeDataVersionsHelpMessage

parseArguments :: Parser Arguments
parseArguments = Arguments <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand =
  hsubparser $
    mconcat
      [ command "check" parseCommandCheck,
        command "next" parseCommandNext,
        command "sample" parseCommandSample,
        command "schedule" parseCommandSchedule
      ]

parseCommandCheck :: ParserInfo Command
parseCommandCheck = info parser modifier
  where
    modifier = fullDesc <> progDesc "Check that all schedules are sensible"
    parser = pure CommandCheck

parseCommandNext :: ParserInfo Command
parseCommandNext = info parser modifier
  where
    modifier = fullDesc <> progDesc "List the next times that scheduled will be activated"
    parser = pure CommandNext

parseCommandSample :: ParserInfo Command
parseCommandSample = info parser modifier
  where
    modifier = fullDesc <> progDesc "Produce a sample scheduled project being filled in"
    parser =
      CommandSample
        <$> strArgument
          ( mconcat
              [ help "template to fill in",
                metavar "FILEPATH",
                completer $ bashCompleter "file"
              ]
          )
        <*> optional
          ( strOption
              ( mconcat
                  [ long "destination",
                    help "destination path template. Note that the rendered template will be written here",
                    metavar "FILEPATH_TEMPLATE",
                    completer $ bashCompleter "file"
                  ]
              )
          )

parseCommandSchedule :: ParserInfo Command
parseCommandSchedule = info parser modifier
  where
    modifier = fullDesc <> progDesc "Run the schedules"
    parser = pure CommandSchedule

parseFlags :: Parser (Report.FlagsWithConfigFile Flags)
parseFlags =
  Report.parseFlagsWithConfigFile $
    Flags <$> Report.parseDirectoryFlags
      <*> optional
        ( strOption
            ( mconcat
                [ long "state-file",
                  help "The state file to use",
                  metavar "FILEPATH",
                  completer $ bashCompleter "file"
                ]
            )
        )
