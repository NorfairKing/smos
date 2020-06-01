{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Report.OptParse where

import Control.Arrow
import Control.Monad
import Data.Aeson as JSON (eitherDecodeFileStrict)
import Data.Aeson (FromJSON)
import Data.Maybe
import qualified Data.Text as T
import Data.Yaml as Yaml (decodeFileEither, prettyPrintParseException)
import Options.Applicative
import Path
import Path.IO
import Smos.Report.Config
import Smos.Report.OptParse.Types
import qualified System.Environment as System
import YamlParse.Applicative hiding (Parser)

combineToConfig ::
  SmosReportConfig -> Flags -> Environment -> Maybe Configuration -> IO SmosReportConfig
combineToConfig src Flags {..} Environment {..} mc = do
  smosReportConfigDirectoryConfig <- combineToDirectoryConfig (smosReportConfigDirectoryConfig src) flagDirectoryFlags envDirectoryEnvironment (mc >>= confDirectoryConf)
  smosReportConfigWorkConfig <- combineToWorkReportConfig (smosReportConfigWorkConfig src) (mc >>= confWorkReportConf)
  pure $ SmosReportConfig {..}

combineToDirectoryConfig :: DirectoryConfig -> DirectoryFlags -> DirectoryEnvironment -> Maybe DirectoryConfiguration -> IO DirectoryConfig
combineToDirectoryConfig dc DirectoryFlags {..} DirectoryEnvironment {..} mc = do
  wfs <-
    case msum [dirFlagWorkflowDir, dirEnvWorkflowDir, mc >>= (fmap T.unpack . directoryConfWorkflowDir)] of
      Nothing -> pure $ directoryConfigWorkflowFileSpec dc
      Just wd -> do
        ad <- resolveDir' wd
        pure $ DirAbsolute ad
  afs <-
    case msum [dirFlagArchiveDir, dirEnvArchiveDir, mc >>= (fmap T.unpack . directoryConfArchiveDir)] of
      Nothing -> pure $ directoryConfigArchiveFileSpec dc
      Just wd -> do
        ad <- resolveDir' wd
        pure $ ArchiveAbsolute ad
  pfs <-
    case msum [dirFlagProjectsDir, dirEnvProjectsDir, mc >>= (fmap T.unpack . directoryConfProjectsDir)] of
      Nothing -> pure $ directoryConfigProjectsFileSpec dc
      Just wd -> do
        ad <- resolveDir' wd
        pure $ ProjectsAbsolute ad
  apfs <-
    case msum
      [ dirFlagArchivedProjectsDir,
        dirEnvArchivedProjectsDir,
        mc >>= (fmap T.unpack . directoryConfArchivedProjectsDir)
      ] of
      Nothing -> pure $ directoryConfigArchivedProjectsFileSpec dc
      Just wd -> do
        ad <- resolveDir' wd
        pure $ ArchivedProjectsAbsolute ad
  pure $
    dc
      { directoryConfigWorkflowFileSpec = wfs,
        directoryConfigArchiveFileSpec = afs,
        directoryConfigProjectsFileSpec = pfs,
        directoryConfigArchivedProjectsFileSpec = apfs
      }

combineToWorkReportConfig :: WorkReportConfig -> Maybe WorkReportConfiguration -> IO WorkReportConfig
combineToWorkReportConfig wrc mc =
  pure $
    wrc
      { workReportConfigWorkBaseFilter =
          (mc >>= workReportConfWorkBaseFilter) <|> workReportConfigWorkBaseFilter wrc,
        workReportConfigContexts = fromMaybe (workReportConfigContexts wrc) (mc >>= workReportConfContexts)
      }

parseFlags :: Parser Flags
parseFlags =
  Flags <$> parseDirectoryFlags

parseFlagsWithConfigFile :: Parser a -> Parser (FlagsWithConfigFile a)
parseFlagsWithConfigFile p =
  FlagsWithConfigFile <$> parseConfigFileFlag <*> p

parseConfigFileFlag :: Parser (Maybe FilePath)
parseConfigFileFlag =
  option
    (Just <$> str)
    (mconcat [metavar "FILEPATH", help "The config file to use", long "config-file", value Nothing])

parseDirectoryFlags :: Parser DirectoryFlags
parseDirectoryFlags =
  DirectoryFlags <$> parseWorkflowDirFlag <*> parseArchiveDirFlag
    <*> parseProjectsDirFlag
    <*> parseArchivedProjectsDirFlag

parseWorkflowDirFlag :: Parser (Maybe FilePath)
parseWorkflowDirFlag =
  option
    (Just <$> str)
    ( mconcat
        [ metavar "FILEPATH",
          help "The workflow directory to use",
          long "workflow-dir",
          value Nothing
        ]
    )

parseArchiveDirFlag :: Parser (Maybe FilePath)
parseArchiveDirFlag =
  option
    (Just <$> str)
    ( mconcat
        [metavar "FILEPATH", help "The archive directory to use", long "archive-dir", value Nothing]
    )

parseProjectsDirFlag :: Parser (Maybe FilePath)
parseProjectsDirFlag =
  option
    (Just <$> str)
    ( mconcat
        [ metavar "FILEPATH",
          help "The projects directory to use",
          long "projects-dir",
          value Nothing
        ]
    )

parseArchivedProjectsDirFlag :: Parser (Maybe FilePath)
parseArchivedProjectsDirFlag =
  option
    (Just <$> str)
    ( mconcat
        [ metavar "FILEPATH",
          help "The archived projects directory to use",
          long "archived-projects-dir",
          value Nothing
        ]
    )

getEnvironment :: IO Environment
getEnvironment = do
  envDirectoryEnvironment <- getDirectoryEnvironment
  pure Environment {..}

getDirectoryEnvironment :: IO DirectoryEnvironment
getDirectoryEnvironment = do
  env <- System.getEnvironment
  let getSmosEnv :: String -> Maybe String
      getSmosEnv = getSmosEnvVar env
  pure
    DirectoryEnvironment
      { dirEnvWorkflowDir =
          msum $ map getSmosEnv ["WORKFLOW_DIRECTORY", "WORKFLOW_DIR", "WORKFLOW_DIR"],
        dirEnvArchiveDir = msum $ map getSmosEnv ["ARCHIVE_DIRECTORY", "ARCHIVE_DIR", "ARCHIVE_DIR"],
        dirEnvProjectsDir =
          msum $ map getSmosEnv ["PROJECTS_DIRECTORY", "PROJECTS_DIR", "PROJECTS_DIR"],
        dirEnvArchivedProjectsDir =
          msum $
            map
              getSmosEnv
              ["ARCHIVED_PROJECTS_DIRECTORY", "ARCHIVED_PROJECTS_DIR", "ARCHIVED_PROJECTS_DIR"]
      }

getEnvWithConfigFile :: IO a -> IO (EnvWithConfigFile a)
getEnvWithConfigFile func = do
  env <- System.getEnvironment
  let envWithConfigFile = msum $ map (getSmosEnvVar env) ["CONFIGURATION_FILE", "CONFIG_FILE", "CONFIG"]
  envWithRestEnv <- func
  pure EnvWithConfigFile {..}

getSmosEnvVar :: [(String, String)] -> String -> Maybe String
getSmosEnvVar env key = ("SMOS_" ++ key) `lookup` env

defaultConfigFiles :: IO [Path Abs File]
defaultConfigFiles = do
  home <- getHomeDir
  homeConfigDir <- resolveDir home ".smos"
  xdgConfigDir <- getXdgDir XdgConfig (Just [reldir|smos|])
  let inDirs = do
        d <- [xdgConfigDir, homeConfigDir]
        pure $ d </> [relfile|config|]
  plainFile <- resolveFile home ".smos"
  let files = inDirs ++ [plainFile]
  pure $ mapMaybe (setFileExtension ".yaml") files

parseYamlConfig :: FromJSON a => Path Abs File -> IO (Either String a)
parseYamlConfig configFile =
  fmap (left prettyPrintParseException) $ decodeFileEither $ fromAbsFile configFile

parseJSONConfig :: FromJSON a => Path Abs File -> IO (Either String a)
parseJSONConfig configFile = JSON.eitherDecodeFileStrict $ fromAbsFile configFile

getConfiguration :: (FromJSON a, YamlSchema a) => FlagsWithConfigFile b -> EnvWithConfigFile c -> IO (Maybe a)
getConfiguration FlagsWithConfigFile {..} EnvWithConfigFile {..} = do
  case flagWithConfigFile <|> envWithConfigFile of
    Just sf -> resolveFile' sf >>= readConfigFile
    Nothing -> defaultConfigFiles >>= readFirstConfigFile
