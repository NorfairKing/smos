{-# LANGUAGE OverloadedStrings #-}

module Smos.GitHub.OptParse where

import Data.Version
import qualified Env
import Options.Applicative
import Paths_smos_github
import Smos.GitHub.OptParse.Types
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
  let d = case cmd of
        CommandList -> DispatchList
  setDirectorySettings <-
    Report.combineToDirectoryConfig
      Report.defaultDirectoryConfig
      flagDirectoryFlags
      envDirectoryEnvironment
      (confDirectoryConfiguration <$> mc)
  pure (Instructions d Settings {..})

-- where
--   cM :: (GitHubConfiguration -> Maybe a) -> Maybe a
--   cM func = mc >>= confGitHubConfiguration >>= func

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

-- where
--   mE = Env.def Nothing <> Env.keep

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
    description = "Smos Scheduler Tool version " <> showVersion version

parseArguments :: Parser Arguments
parseArguments = Arguments <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand =
  hsubparser $
    mconcat
      [ command "list" parseCommandList
      ]

parseCommandList :: ParserInfo Command
parseCommandList = info parser modifier
  where
    modifier = fullDesc <> progDesc "List the relevant github issues"
    parser = pure CommandList

parseFlags :: Parser (Report.FlagsWithConfigFile Flags)
parseFlags =
  Report.parseFlagsWithConfigFile $
    Flags <$> Report.parseDirectoryFlags
