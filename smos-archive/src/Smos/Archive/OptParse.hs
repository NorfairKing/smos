{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Archive.OptParse
  ( module Smos.Archive.OptParse,
    module Smos.Archive.OptParse.Types,
  )
where

import qualified Env
import Options.Applicative
import Path.IO
import Smos.Archive.OptParse.Types
import qualified Smos.Report.Config as Report
import qualified Smos.Report.OptParse as Report
import qualified System.Environment as System

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
      defaultPrefs
        { prefShowHelpOnError = True,
          prefShowHelpOnEmpty = True
        }

flagsParser :: ParserInfo (Report.FlagsWithConfigFile Flags)
flagsParser = info (helper <*> Report.parseFlagsWithConfigFile parseFlags) help_
  where
    help_ = fullDesc <> progDesc description
    description = "smos-archive"

parseFlags :: Parser Flags
parseFlags =
  Flags <$> strArgument (mconcat [help "The file to archive", metavar "FILEPATH", action "file"])
    <*> Report.parseDirectoryFlags

getEnvironment :: IO (Report.EnvWithConfigFile Environment)
getEnvironment = Env.parse (Env.header "Environment") prefixedEnvironmentParser

prefixedEnvironmentParser :: Env.Parser Env.Error (Report.EnvWithConfigFile Environment)
prefixedEnvironmentParser = Env.prefixed "SMOS_" environmentParser

environmentParser :: Env.Parser Env.Error (Report.EnvWithConfigFile Environment)
environmentParser = Report.envWithConfigFileParser $ Environment <$> Report.directoryEnvironmentParser
