{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Single.OptParse
  ( module Smos.Single.OptParse,
    module Smos.Single.OptParse.Types,
  )
where

import Control.Monad
import qualified Data.Text as T
import Data.Version
import qualified Env
import Options.Applicative
import Options.Applicative.Help.Pretty as Doc
import Path
import Paths_smos_single
import Smos.CLI.OptParse as CLI
import Smos.Data
import qualified Smos.Report.Config as Report
import qualified Smos.Report.OptParse as Report
import Smos.Single.OptParse.Types
import qualified System.Environment as System
import System.Exit

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  env <- getEnvironment
  config <- getConfiguration flags env
  deriveSettings (flagWithRestFlags flags) (envWithRestEnv env) config

deriveSettings :: Flags -> Environment -> Maybe Configuration -> IO Settings
deriveSettings Flags {..} Environment {..} mc = do
  setTask <-
    case parseHeader $ T.pack $ unwords flagTaskPieces of
      Left err -> die $ "Failed to parse header: " <> err
      Right h -> pure h
  setDirectorySettings <-
    Report.combineToDirectoryConfig
      Report.defaultDirectoryConfig
      flagDirectoryFlags
      envDirectoryEnvironment
      (confDirectoryConfiguration <$> mc)
  setTaskFile <- forM flagTaskFile parseRelFile
  pure Settings {..}

getFlags :: IO (FlagsWithConfigFile Flags)
getFlags = do
  args <- System.getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult (FlagsWithConfigFile Flags)
runArgumentsParser = CLI.execOptionParserPure flagsParser

flagsParser :: ParserInfo (FlagsWithConfigFile Flags)
flagsParser = info (helper <*> parseFlags) help_
  where
    help_ = fullDesc <> progDescDoc (Just description)
    description :: Doc
    description =
      Doc.vsep $
        map Doc.text $
          [ "",
            "Smos Single-task Tool version: " <> showVersion version,
            ""
          ]
            ++ writeDataVersionsHelpMessage

parseFlags :: Parser (FlagsWithConfigFile Flags)
parseFlags =
  parseFlagsWithConfigFile $
    Flags
      <$> some
        ( strArgument
            ( mconcat
                [ help "The task. Pass any number of arguments and they will be interpreted as the task together.",
                  metavar "TASK"
                ]
            )
        )
      <*> optional
        ( strOption
            ( mconcat
                [ long "file",
                  help "The file to put the task in",
                  metavar "FILEPATH"
                ]
            )
        )
      <*> Report.parseDirectoryFlags

getEnvironment :: IO (EnvWithConfigFile Environment)
getEnvironment = Env.parse (Env.header "Environment") prefixedEnvironmentParser

prefixedEnvironmentParser :: Env.Parser Env.Error (EnvWithConfigFile Environment)
prefixedEnvironmentParser = Env.prefixed "SMOS_" environmentParser

environmentParser :: Env.Parser Env.Error (EnvWithConfigFile Environment)
environmentParser =
  envWithConfigFileParser $
    Environment <$> Report.directoryEnvironmentParser
