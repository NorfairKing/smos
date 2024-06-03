{-# LANGUAGE OverloadedStrings #-}
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
import Smos.CLI.Colour
import Smos.CLI.OptParse as CLI
import Smos.Data
import Smos.Directory.OptParse
import Smos.Scheduler.OptParse.Types
import qualified System.Environment as System

getInstructions :: IO Instructions
getInstructions = do
  (Arguments cmd flags) <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions cmd (flagWithRestFlags flags) (envWithRestEnv env) config

combineToInstructions :: Command -> Flags -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions cmd Flags {..} Environment {..} mc = do
  d <- case cmd of
    CommandCheck -> pure DispatchCheck
    CommandNext -> pure DispatchNext
    CommandSample fp mdpt -> DispatchSample <$> resolveFile' fp <*> mapM (fmap DestinationPathTemplate . parseRelFile) mdpt
    CommandSchedule -> pure DispatchSchedule
  setDirectorySettings <-
    combineToDirectorySettings
      defaultDirectorySettings
      flagDirectoryFlags
      envDirectoryEnvironment
      (confDirectoryConfiguration <$> mc)
  let setSchedule = fromMaybe (Schedule []) $ cM schedulerConfSchedule
  let setColourSettings = getColourSettings (mc >>= confColourConfiguration)
  pure (Instructions d Settings {..})
  where
    cM :: (SchedulerConfiguration -> Maybe a) -> Maybe a
    cM func = mc >>= confSchedulerConfiguration >>= func

getEnvironment :: IO (EnvWithConfigFile Environment)
getEnvironment = Env.parse (Env.header "Environment") prefixedEnvironmentParser

prefixedEnvironmentParser :: Env.Parser Env.Error (EnvWithConfigFile Environment)
prefixedEnvironmentParser = Env.prefixed "SMOS_" environmentParser

environmentParser :: Env.Parser Env.Error (EnvWithConfigFile Environment)
environmentParser =
  envWithConfigFileParser $
    Environment <$> directoryEnvironmentParser

getArguments :: IO Arguments
getArguments = do
  args <- System.getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = CLI.execOptionParserPure argumentsParser

argumentsParser :: ParserInfo Arguments
argumentsParser = info (helper <*> parseArguments) help_
  where
    help_ = fullDesc <> progDescDoc (Just description)
    description :: Doc
    description =
      Doc.vsep $
        map Doc.pretty $
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

parseFlags :: Parser (FlagsWithConfigFile Flags)
parseFlags =
  parseFlagsWithConfigFile $
    Flags
      <$> parseDirectoryFlags
