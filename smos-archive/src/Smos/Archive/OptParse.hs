{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Archive.OptParse
  ( module Smos.Archive.OptParse,
    module Smos.Archive.OptParse.Types,
  )
where

import Data.Version
import qualified Env
import Options.Applicative
import Options.Applicative.Help.Pretty as Doc
import Path.IO
import Paths_smos_archive
import Smos.Archive.OptParse.Types
import Smos.Data
import qualified Smos.Report.Config as Report
import qualified Smos.Report.OptParse as Report
import qualified System.Environment as System

getInstructions :: IO Instructions
getInstructions = do
  Arguments c flags <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions c (Report.flagWithRestFlags flags) (Report.envWithRestEnv env) config

getConfig :: Report.FlagsWithConfigFile Flags -> Report.EnvWithConfigFile Environment -> IO (Maybe Configuration)
getConfig f e =
  fmap Configuration <$> Report.getConfiguration f e

combineToInstructions ::
  Command -> Flags -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions c Flags {..} Environment {..} mc = do
  dispatch <- case c of
    CommandArchiveFile filepath -> do
      file <- resolveFile' filepath
      pure $ DispatchArchiveFile file
  settings <- do
    setDirectorySettings <-
      Report.combineToDirectoryConfig
        Report.defaultDirectoryConfig
        flagDirectoryFlags
        envDirectoryEnvironment
        (confDirectoryConfiguration <$> mc)
    pure $ Settings {..}
  pure $ Instructions dispatch settings

getArguments :: IO Arguments
getArguments = do
  args <- System.getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = execParserPure prefs_ argParser
  where
    prefs_ =
      defaultPrefs
        { prefShowHelpOnError = True,
          prefShowHelpOnEmpty = True
        }

argParser :: ParserInfo Arguments
argParser = info (helper <*> parseArgs) help_
  where
    help_ = fullDesc <> progDescDoc (Just description)
    description :: Doc
    description =
      Doc.vsep $
        map Doc.text $
          [ "",
            "Smos Archive Tool version: " <> showVersion version,
            ""
          ]
            ++ readWriteDataVersionsHelpMessage

parseArgs :: Parser Arguments
parseArgs = Arguments <$> parseCommand <*> Report.parseFlagsWithConfigFile parseFlags

parseCommand :: Parser Command
parseCommand =
  hsubparser
    ( mconcat
        [ command "file" parseCommandArchiveFile
        ]
    )
    <|> infoParser parseCommandArchiveFile

parseCommandArchiveFile :: ParserInfo Command
parseCommandArchiveFile = info parser modifier
  where
    modifier = fullDesc <> progDesc "Select entries based on a given filter"
    parser =
      CommandArchiveFile
        <$> strArgument (mconcat [help "The file to archive", metavar "FILEPATH", action "file"])

parseFlags :: Parser Flags
parseFlags = Flags <$> Report.parseDirectoryFlags

getEnvironment :: IO (Report.EnvWithConfigFile Environment)
getEnvironment = Env.parse (Env.header "Environment") prefixedEnvironmentParser

prefixedEnvironmentParser :: Env.Parser Env.Error (Report.EnvWithConfigFile Environment)
prefixedEnvironmentParser = Env.prefixed "SMOS_" environmentParser

environmentParser :: Env.Parser Env.Error (Report.EnvWithConfigFile Environment)
environmentParser = Report.envWithConfigFileParser $ Environment <$> Report.directoryEnvironmentParser

getConfiguration :: Report.FlagsWithConfigFile Flags -> Report.EnvWithConfigFile Environment -> IO (Maybe Configuration)
getConfiguration = Report.getConfiguration
