{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Archive.OptParse
  ( module Smos.Archive.OptParse,
    module Smos.Archive.OptParse.Types,
  )
where

import Data.Maybe
import Data.Version
import qualified Env
import Options.Applicative
import Options.Applicative.Help.Pretty as Doc
import Path.IO
import Paths_smos_archive
import Smos.Archive.OptParse.Types
import Smos.CLI.Logging
import Smos.CLI.OptParse as CLI
import Smos.Data
import Smos.Directory.OptParse
import Smos.Report.OptParse (parseFileFilterArgs, parsePeriod)
import Smos.Report.Period
import qualified System.Environment as System

getInstructions :: IO Instructions
getInstructions = do
  Arguments c flags <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions c (flagWithRestFlags flags) (envWithRestEnv env) config

combineToInstructions ::
  Command -> Flags -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions c Flags {..} Environment {..} mc = do
  dispatch <- case c of
    CommandFile filepath -> do
      file <- resolveFile' filepath
      pure $ DispatchFile file
    CommandExport ExportFlags {..} -> do
      exportSetExportDir <- resolveDir' exportFlagExportDir
      let exportSetFilter = exportFlagFilter
      let exportSetPeriod = fromMaybe AllTime exportFlagPeriod
      let exportSetAlsoDeleteOriginals = fromMaybe False exportFlagAlsoDeleteOriginals
      pure $ DispatchExport ExportSettings {..}
  settings <- do
    setDirectorySettings <-
      combineToDirectorySettings
        defaultDirectorySettings
        flagDirectoryFlags
        envDirectoryEnvironment
        (confDirectoryConfiguration <$> mc)
    let setLogLevel = combineLogLevelSettings flagLogLevel envLogLevel (mc >>= confLogLevel)
    pure $ Settings {..}
  pure $ Instructions dispatch settings

getArguments :: IO Arguments
getArguments = do
  args <- System.getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = CLI.execOptionParserPure argParser

argParser :: ParserInfo Arguments
argParser = info (helper <*> parseArgs) help_
  where
    help_ = fullDesc <> progDescDoc (Just description)
    description :: Doc
    description =
      Doc.vsep $
        map Doc.pretty $
          [ "",
            "Smos Archive Tool version: " <> showVersion version,
            ""
          ]
            ++ readWriteDataVersionsHelpMessage

parseArgs :: Parser Arguments
parseArgs =
  Arguments
    <$> parseCommand
    <*> parseFlagsWithConfigFile parseFlags

parseCommand :: Parser Command
parseCommand =
  hsubparser
    ( mconcat
        [ command "file" parseCommandFile,
          command "export" parseCommandExport
        ]
    )
    <|> infoParser parseCommandFile

parseCommandFile :: ParserInfo Command
parseCommandFile = info parser modifier
  where
    modifier = fullDesc <> progDesc "Archive a single file"
    parser =
      CommandFile
        <$> strArgument
          ( mconcat
              [ help "The file to archive",
                metavar "FILEPATH",
                action "file"
              ]
          )

parseCommandExport :: ParserInfo Command
parseCommandExport = info parser modifier
  where
    modifier = fullDesc <> progDesc "Export (a portion of) an archive"
    parser =
      CommandExport
        <$> ( ExportFlags
                <$> strArgument
                  ( mconcat
                      [ help "The directory to export the archive to",
                        metavar "FILEPATH",
                        action "directory"
                      ]
                  )
                <*> parseFileFilterArgs
                <*> parsePeriod
                <*> optional
                  ( switch
                      ( mconcat
                          [ help "Also delete the originals from the archive",
                            long "also-delete-originals"
                          ]
                      )
                  )
            )

parseFlags :: Parser Flags
parseFlags =
  Flags
    <$> parseDirectoryFlags
    <*> parseLogLevelOption

getEnvironment :: IO (EnvWithConfigFile Environment)
getEnvironment = Env.parse (Env.header "Environment") prefixedEnvironmentParser

prefixedEnvironmentParser :: Env.Parser Env.Error (EnvWithConfigFile Environment)
prefixedEnvironmentParser = Env.prefixed "SMOS_" environmentParser

environmentParser :: Env.Parser Env.Error (EnvWithConfigFile Environment)
environmentParser =
  envWithConfigFileParser $
    Environment
      <$> directoryEnvironmentParser
      <*> optional (Env.var logLevelEnvParser "LOG_LEVEL" (Env.help "The minimal severity of log messages"))
