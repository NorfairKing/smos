{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.ASCIInema.OptParse
  ( module Smos.ASCIInema.OptParse,
    module Smos.ASCIInema.OptParse.Types,
  )
where

import Data.Maybe
import qualified Env
import Options.Applicative
import Path
import Path.IO
import Smos.ASCIInema.OptParse.Types
import qualified Smos.Report.Config as Report
import qualified Smos.Report.OptParse as Report
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
    CommandRecord fp -> DispatchRecord <$> resolveFile' fp
  setDirectorySettings <-
    Report.combineToDirectoryConfig
      Report.defaultDirectoryConfig
      flagDirectoryFlags
      envDirectoryEnvironment
      (confDirectoryConfiguration <$> mc)
  pure (Instructions d Settings {..})
  where
    cM :: (ASCIInemaConfiguration -> Maybe a) -> Maybe a
    cM func = mc >>= confASCIInemaConfiguration >>= func

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
      ParserPrefs
        { prefMultiSuffix = "",
          prefDisambiguate = True,
          prefShowHelpOnError = True,
          prefShowHelpOnEmpty = True,
          prefBacktrack = True,
          prefColumns = 80
        }

argumentsParser :: ParserInfo Arguments
argumentsParser = info (helper <*> parseArguments) help_
  where
    help_ = fullDesc <> progDesc description
    description = "smos-scheduler"

parseArguments :: Parser Arguments
parseArguments = Arguments <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand =
  hsubparser $
    mconcat
      [ command "record" parseCommandRecord
      ]

parseCommandRecord :: ParserInfo Command
parseCommandRecord = info parser modifier
  where
    modifier = fullDesc <> progDesc "Record an asciinema"
    parser =
      CommandRecord
        <$> strArgument
          ( mconcat
              [ help "The instructions file",
                metavar "FILEPATH"
              ]
          )

parseFlags :: Parser (Report.FlagsWithConfigFile Flags)
parseFlags =
  Report.parseFlagsWithConfigFile $
    Flags <$> Report.parseDirectoryFlags
