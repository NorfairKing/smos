{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Report.OptParse
  ( module Smos.Report.OptParse,
    module Smos.Report.OptParse.Types,
  )
where

import Control.Arrow
import Control.Monad
import Data.Aeson (FromJSON)
import Data.Aeson as JSON (eitherDecodeFileStrict)
import Data.Maybe
import qualified Data.Text as T
import Data.Yaml as Yaml (decodeFileEither, prettyPrintParseException)
import qualified Env
import Options.Applicative
import Path
import Path.IO
import Smos.Report.Config
import Smos.Report.OptParse.Types
import YamlParse.Applicative hiding (Parser)

combineToConfig ::
  SmosReportConfig -> Flags -> Environment -> Maybe Configuration -> IO SmosReportConfig
combineToConfig src Flags {..} Environment {..} mc = do
  smosReportConfigDirectoryConfig <- combineToDirectoryConfig (smosReportConfigDirectoryConfig src) flagDirectoryFlags envDirectoryEnvironment (confDirectoryConf <$> mc)
  smosReportConfigWorkConfig <- combineToWorkReportConfig (smosReportConfigWorkConfig src) (mc >>= confWorkReportConf)
  pure $ SmosReportConfig {..}

combineToDirectoryConfig :: DirectoryConfig -> DirectoryFlags -> DirectoryEnvironment -> Maybe DirectoryConfiguration -> IO DirectoryConfig
combineToDirectoryConfig dc DirectoryFlags {..} DirectoryEnvironment {..} mc = do
  wfs <-
    case msum [dirFlagWorkflowDir, dirEnvWorkflowDir, mc >>= (fmap T.unpack . directoryConfWorkflowDir)] of
      Nothing -> pure $ directoryConfigWorkflowFileSpec dc
      Just wd -> do
        ad <- resolveDir' wd
        pure $ AbsoluteWorkflow ad
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
      { workReportConfigBaseFilter =
          (mc >>= workReportConfBaseFilter) <|> workReportConfigBaseFilter wrc,
        workReportConfigChecks = fromMaybe (workReportConfigChecks wrc) (mc >>= workReportConfChecks),
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
    ( mconcat
        [ metavar "FILEPATH",
          help "The config file to use",
          long "config-file",
          value Nothing,
          completer $ bashCompleter "file"
        ]
    )

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
          value Nothing,
          completer $ bashCompleter "directory"
        ]
    )

parseArchiveDirFlag :: Parser (Maybe FilePath)
parseArchiveDirFlag =
  option
    (Just <$> str)
    ( mconcat
        [ metavar "FILEPATH",
          help "The archive directory to use",
          long "archive-dir",
          value Nothing,
          completer $ bashCompleter "directory"
        ]
    )

parseProjectsDirFlag :: Parser (Maybe FilePath)
parseProjectsDirFlag =
  option
    (Just <$> str)
    ( mconcat
        [ metavar "FILEPATH",
          help "The projects directory to use",
          long "projects-dir",
          value Nothing,
          completer $ bashCompleter "directory"
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
          value Nothing,
          completer $ bashCompleter "directory"
        ]
    )

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") prefixedEnvironmentParser

prefixedEnvironmentParser :: Env.Parser Env.Error Environment
prefixedEnvironmentParser = Env.prefixed "SMOS_" environmentParser

environmentParser :: Env.Parser Env.Error Environment
environmentParser = Environment <$> directoryEnvironmentParser

directoryEnvironmentParser :: Env.Parser Env.Error DirectoryEnvironment
directoryEnvironmentParser =
  DirectoryEnvironment
    <$> Env.var (fmap Just . Env.str) "WORKFLOW_DIR" (mE <> Env.help "Workflow directory")
    <*> Env.var (fmap Just . Env.str) "ARCHIVE_DIR" (mE <> Env.help "Archive directory")
    <*> Env.var (fmap Just . Env.str) "PROJECTS_DIR" (mE <> Env.help "Projects directory")
    <*> Env.var (fmap Just . Env.str) "ARCHIVED_PROJECTS_DIR" (mE <> Env.help "Archived projects directory")
  where
    mE = Env.def Nothing <> Env.keep

envWithConfigFileParser :: Env.Parser Env.Error a -> Env.Parser Env.Error (EnvWithConfigFile a)
envWithConfigFileParser p =
  EnvWithConfigFile
    <$> Env.var (fmap Just . Env.str) "CONFIG_FILE" (Env.def Nothing <> Env.keep <> Env.help "Workflow directory")
    <*> p

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
  pure $ mapMaybe (replaceExtension ".yaml") files

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
