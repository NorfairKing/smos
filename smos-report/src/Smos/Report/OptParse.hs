{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.OptParse where

import qualified System.Environment as System
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
     SmosReportConfig -> Flags -> Environment -> Maybe Configuration -> IO SmosReportConfig
combineToConfig src Flags {..} Environment {..} mc = do
  wfs <-
    case msum [flagWorkflowDir, envWorkflowDir, mc >>= confWorkflowDir] of
      Nothing -> pure $ smosReportConfigAgendaFileSpec src
      Just wd -> do
        ad <- resolveDir' wd
        pure $ DirAbsolute ad
  afs <-
    case msum [flagArchiveDir, envArchiveDir, mc >>= confArchiveDir] of
      Nothing -> pure $ smosReportConfigArchiveFileSpec src
      Just wd -> do
        ad <- resolveDir' wd
        pure $ ArchiveAbsolute ad
  pure $
    SmosReportConfig {smosReportConfigAgendaFileSpec = wfs, smosReportConfigArchiveFileSpec = afs}

parseFlags :: Parser Flags
parseFlags = Flags <$> parseConfigFileFlag <*> parseWorkflowDirFlag <*> parseArchiveDirFlag

parseConfigFileFlag :: Parser (Maybe FilePath)
parseConfigFileFlag =
  option
    (Just <$> str)
    (mconcat [metavar "FILEPATH", help "The config file to use", long "config-file", value Nothing])

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

parseArchiveDirFlag :: Parser (Maybe FilePath)
parseArchiveDirFlag =
  option
    (Just <$> str)
    (mconcat
       [metavar "FILEPATH", help "The archive directory to use", long "archive-dir", value Nothing])

getEnvironment :: IO Environment
getEnvironment = do
  env <- System.getEnvironment
  let getSmosEnv :: String -> Maybe String
      getSmosEnv key = ("SMOS_" ++ key) `lookup` env
  pure
    Environment
      { envConfigFile = msum $ map getSmosEnv ["CONFIGURATION_FILE", "CONFIG_FILE", "CONFIG"]
      , envWorkflowDir =
          msum $ map getSmosEnv ["WORKFLOW_DIRECTORY", "WORKFLOW_DIR", "WORKFLOW_DIR"]
      , envArchiveDir = msum $ map getSmosEnv ["ARCHIVE_DIRECTORY", "ARCHIVE_DIR", "ARCHIVE_DIR"]
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
  fmap (left prettyPrintParseException) $ decodeFileEither $ fromAbsFile configFile

parseJSONConfig :: FromJSON a => Path Abs File -> IO (Either String a)
parseJSONConfig configFile = do
  JSON.eitherDecodeFileStrict $ fromAbsFile configFile

getConfiguration :: FromJSON a => Flags -> Environment -> IO (Maybe a)
getConfiguration Flags {..} Environment {..} = do
  mConfigFile <-
    case msum [flagConfigFile, envConfigFile] of
      Nothing -> msum <$> Control.Monad.sequence [defaultYamlConfigFile, defaultJSONConfigFile]
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
