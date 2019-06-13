{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.OptParse where

import System.Environment
import System.Exit

import Control.Arrow
import Control.Monad

import Data.Aeson as JSON (eitherDecodeFileStrict)
import Data.Aeson (FromJSON)
import Data.Yaml as Yaml (decodeFileEither, prettyPrintParseException)

import Path
import Path.IO

import Smos.Report.Config

import Options.Applicative

import Smos.Report.OptParse.Types

combineToConfig ::
     SmosReportConfig
  -> Flags
  -> Environment
  -> Maybe Configuration
  -> IO SmosReportConfig
combineToConfig src Flags {..} Environment {..} mc = do
  pure $
    case msum [flagWorkflowDir, envWorkflowDir, mc >>= confWorkflowDir] of
      Nothing -> src
      Just wd ->
        src {smosReportConfigAgendaFileSpec = AgendaFileSpec $ resolveDir' wd}

parseFlags :: Parser Flags
parseFlags = Flags <$> parseWorkflowDirFlag

parseWorkflowDirFlag :: Parser (Maybe FilePath)
parseWorkflowDirFlag =
  option
    (Just <$> str)
    (mconcat
       [ metavar "FILEPATH"
       , help "The workflow directory to use"
       , long "workflow-dir"
       , value Nothing
       ])

getEnv :: IO Environment
getEnv = do
  env <- getEnvironment
  let getSmosEnv :: String -> Maybe String
      getSmosEnv key = ("SMOS_" ++ key) `lookup` env
  pure
    Environment
      { envWorkflowDir =
          getSmosEnv "WORKFLOW_DIRECTORY" <|> getSmosEnv "WORKFLOW_DIR"
      }

defaultJSONConfigFile :: IO (Maybe (Path Abs File))
defaultJSONConfigFile = do
  home <- getHomeDir
  p <- resolveFile home ".smos.json"
  e <- doesFileExist p
  pure $
    if e
      then Just p
      else Nothing

defaultYamlConfigFile :: IO (Maybe (Path Abs File))
defaultYamlConfigFile = do
  home <- getHomeDir
  p <- resolveFile home ".smos.yaml"
  e <- doesFileExist p
  pure $
    if e
      then Just p
      else Nothing

parseYamlConfig :: FromJSON a => Path Abs File -> IO (Either String a)
parseYamlConfig configFile =
  fmap (left prettyPrintParseException) $
  decodeFileEither $ fromAbsFile configFile

parseJSONConfig :: FromJSON a => Path Abs File -> IO (Either String a)
parseJSONConfig configFile = do
  JSON.eitherDecodeFileStrict $ fromAbsFile configFile

getConfigurationWith :: FromJSON a => [Maybe FilePath] -> IO (Maybe a)
getConfigurationWith mConfigFileOverrides = do
  mConfigFile <-
    case msum mConfigFileOverrides of
      Nothing ->
        msum <$>
        Control.Monad.sequence [defaultYamlConfigFile, defaultJSONConfigFile]
      Just fp -> Just <$> resolveFile' fp
  forM mConfigFile $ \configFile -> do
    errOrConfig <-
      case fileExtension configFile of
        ".json" -> parseJSONConfig configFile
                -- As Yaml
        ".yaml" -> parseYamlConfig configFile
        _ -> parseYamlConfig configFile
    case errOrConfig of
      Left err -> die err
      Right conf -> pure conf
