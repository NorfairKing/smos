{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Smos.Directory.OptParse where

import Autodocodec
import Control.Applicative
import Control.Monad
import Data.Validity.Path ()
import qualified Env
import qualified OptEnvConf
import Options.Applicative
import Path
import Path.IO

combineToDirectorySettings :: DirectorySettings -> DirectoryFlags -> DirectoryEnvironment -> Maybe DirectoryConfiguration -> IO DirectorySettings
combineToDirectorySettings dc DirectoryFlags {..} DirectoryEnvironment {..} mc = do
  wfs <-
    combineToWorkflowDirSpec
      (directoryConfigWorkflowFileSpec dc)
      dirFlagWorkflowDir
      dirEnvWorkflowDir
      (mc >>= directoryConfWorkflowDir)
  afs <-
    case msum [dirFlagArchiveDir, dirEnvArchiveDir, mc >>= directoryConfArchiveDir] of
      Nothing -> pure $ directoryConfigArchiveFileSpec dc
      Just wd -> do
        ad <- resolveDir' wd
        pure $ ArchiveAbsolute ad
  pfs <-
    case msum [dirFlagProjectsDir, dirEnvProjectsDir, mc >>= directoryConfProjectsDir] of
      Nothing -> pure $ directoryConfigProjectsFileSpec dc
      Just wd -> do
        ad <- resolveDir' wd
        pure $ ProjectsAbsolute ad
  apfs <-
    case msum [dirFlagArchivedProjectsDir, dirEnvArchivedProjectsDir, mc >>= directoryConfArchivedProjectsDir] of
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

combineToWorkflowDirSpec ::
  WorkflowDirSpec ->
  Maybe FilePath ->
  Maybe FilePath ->
  Maybe FilePath ->
  IO WorkflowDirSpec
combineToWorkflowDirSpec defaultDirSpec flagWorkflowDir envWorkflowDir confWorkflowDir =
  case flagWorkflowDir <|> envWorkflowDir <|> confWorkflowDir of
    Nothing -> pure defaultDirSpec
    Just wd -> AbsoluteWorkflow <$> resolveDir' wd

parseDirectoryFlags :: Parser DirectoryFlags
parseDirectoryFlags =
  DirectoryFlags
    <$> parseWorkflowDirFlag
    <*> parseArchiveDirFlag
    <*> parseProjectsDirFlag
    <*> parseArchivedProjectsDirFlag

parseWorkflowDirFlag :: Parser (Maybe FilePath)
parseWorkflowDirFlag =
  optional
    ( strOption
        ( mconcat
            [ metavar "DIRECTORY_PATH",
              help "The workflow directory to use",
              long "workflow-dir",
              completer $ bashCompleter "directory"
            ]
        )
    )

parseArchiveDirFlag :: Parser (Maybe FilePath)
parseArchiveDirFlag =
  optional
    ( strOption
        ( mconcat
            [ metavar "DIRECTORY_PATH",
              help "The archive directory to use",
              long "archive-dir",
              completer $ bashCompleter "directory"
            ]
        )
    )

parseProjectsDirFlag :: Parser (Maybe FilePath)
parseProjectsDirFlag =
  optional
    ( strOption
        ( mconcat
            [ metavar "DIRECTORY_PATH",
              help "The projects directory to use",
              long "projects-dir",
              completer $ bashCompleter "directory"
            ]
        )
    )

parseArchivedProjectsDirFlag :: Parser (Maybe FilePath)
parseArchivedProjectsDirFlag =
  optional
    ( strOption
        ( mconcat
            [ metavar "DIRECTORY_PATHPATH",
              help "The archived projects directory to use",
              long "archived-projects-dir",
              completer $ bashCompleter "directory"
            ]
        )
    )

directoryEnvironmentParser :: Env.Parser Env.Error DirectoryEnvironment
directoryEnvironmentParser =
  DirectoryEnvironment
    <$> workflowDirEnvParser
    <*> optional (Env.var Env.str "ARCHIVE_DIR" (Env.help "Archive directory"))
    <*> optional (Env.var Env.str "PROJECTS_DIR" (Env.help "Projects directory"))
    <*> optional (Env.var Env.str "ARCHIVED_PROJECTS_DIR" (Env.help "Archived projects directory"))

workflowDirEnvParser :: Env.Parser Env.Error (Maybe FilePath)
workflowDirEnvParser = optional (Env.var Env.str "WORKFLOW_DIR" (Env.help "Workflow directory"))

data DirectoryFlags = DirectoryFlags
  { dirFlagWorkflowDir :: Maybe FilePath,
    dirFlagArchiveDir :: Maybe FilePath,
    dirFlagProjectsDir :: Maybe FilePath,
    dirFlagArchivedProjectsDir :: Maybe FilePath
  }

data DirectoryEnvironment = DirectoryEnvironment
  { dirEnvWorkflowDir :: Maybe FilePath,
    dirEnvArchiveDir :: Maybe FilePath,
    dirEnvProjectsDir :: Maybe FilePath,
    dirEnvArchivedProjectsDir :: Maybe FilePath
  }

data DirectoryConfiguration = DirectoryConfiguration
  { directoryConfWorkflowDir :: !(Maybe FilePath),
    directoryConfArchiveDir :: !(Maybe FilePath),
    directoryConfProjectsDir :: !(Maybe FilePath),
    directoryConfArchivedProjectsDir :: !(Maybe FilePath)
  }

instance HasObjectCodec DirectoryConfiguration where
  objectCodec =
    DirectoryConfiguration
      <$> optionalFieldOrNull "workflow-dir" "The workflow directory" .= directoryConfWorkflowDir
      <*> optionalFieldOrNull "archive-dir" "The archive directory" .= directoryConfArchiveDir
      <*> optionalFieldOrNull "projects-dir" "The projects directory" .= directoryConfProjectsDir
      <*> optionalFieldOrNull "archived-projects-dir" "The archived projects directory" .= directoryConfArchivedProjectsDir

defaultDirectoryConfiguration :: DirectoryConfiguration
defaultDirectoryConfiguration =
  DirectoryConfiguration
    { directoryConfWorkflowDir = Nothing,
      directoryConfArchiveDir = Nothing,
      directoryConfProjectsDir = Nothing,
      directoryConfArchivedProjectsDir = Nothing
    }

data DirectorySettings = DirectorySettings
  { directoryConfigWorkflowFileSpec :: !WorkflowDirSpec,
    directoryConfigArchiveFileSpec :: !ArchiveDirSpec,
    directoryConfigProjectsFileSpec :: !ProjectsDirSpec,
    directoryConfigArchivedProjectsFileSpec :: !ArchivedProjectsDirSpec
  }

instance OptEnvConf.HasParser DirectorySettings where
  settingsParser = parseDirectorySettings

{-# ANN parseDirectorySettings ("NOCOVER" :: String) #-}
parseDirectorySettings :: OptEnvConf.Parser DirectorySettings
parseDirectorySettings = do
  directoryConfigWorkflowFileSpec <- OptEnvConf.settingsParser
  directoryConfigArchiveFileSpec <- OptEnvConf.settingsParser
  directoryConfigProjectsFileSpec <- OptEnvConf.settingsParser
  directoryConfigArchivedProjectsFileSpec <- OptEnvConf.settingsParser
  pure DirectorySettings {..}

defaultDirectorySettings :: DirectorySettings
defaultDirectorySettings =
  DirectorySettings
    { directoryConfigWorkflowFileSpec = defaultWorkflowDirSpec,
      directoryConfigArchiveFileSpec = defaultArchiveDirSpec,
      directoryConfigProjectsFileSpec = defaultProjectsDirSpec,
      directoryConfigArchivedProjectsFileSpec = defaultArchivedProjectsDirSpec
    }

data WorkflowDirSpec
  = WorkflowInHome (Path Rel Dir)
  | AbsoluteWorkflow (Path Abs Dir)

instance OptEnvConf.HasParser WorkflowDirSpec where
  settingsParser =
    OptEnvConf.choice
      [ AbsoluteWorkflow
          <$> OptEnvConf.directoryPathSetting
            [ OptEnvConf.help "The workflow directory",
              OptEnvConf.name "workflow-dir"
            ],
        pure defaultWorkflowDirSpec
      ]

defaultWorkflowDirSpec :: WorkflowDirSpec
defaultWorkflowDirSpec = WorkflowInHome [reldir|workflow|]

data ArchiveDirSpec
  = ArchiveInWorkflow (Path Rel Dir)
  | ArchiveInHome (Path Rel Dir)
  | ArchiveAbsolute (Path Abs Dir)

instance OptEnvConf.HasParser ArchiveDirSpec where
  settingsParser =
    OptEnvConf.choice
      [ ArchiveAbsolute
          <$> OptEnvConf.directoryPathSetting
            [ OptEnvConf.help "The archive directory",
              OptEnvConf.name "archive-dir"
            ],
        pure defaultArchiveDirSpec
      ]

defaultArchiveDirSpec :: ArchiveDirSpec
defaultArchiveDirSpec = ArchiveInWorkflow [reldir|archive|]

data ProjectsDirSpec
  = ProjectsInWorkflow (Path Rel Dir)
  | ProjectsInHome (Path Rel Dir)
  | ProjectsAbsolute (Path Abs Dir)

instance OptEnvConf.HasParser ProjectsDirSpec where
  settingsParser =
    OptEnvConf.choice
      [ ProjectsAbsolute
          <$> OptEnvConf.directoryPathSetting
            [ OptEnvConf.help "The projects directory",
              OptEnvConf.name "projects-dir"
            ],
        pure defaultProjectsDirSpec
      ]

defaultProjectsDirSpec :: ProjectsDirSpec
defaultProjectsDirSpec = ProjectsInWorkflow [reldir|projects|]

data ArchivedProjectsDirSpec
  = ArchivedProjectsInArchive (Path Rel Dir)
  | ArchivedProjectsInHome (Path Rel Dir)
  | ArchivedProjectsAbsolute (Path Abs Dir)

instance OptEnvConf.HasParser ArchivedProjectsDirSpec where
  settingsParser =
    OptEnvConf.choice
      [ ArchivedProjectsAbsolute
          <$> OptEnvConf.directoryPathSetting
            [ OptEnvConf.help "The archived projects directory",
              OptEnvConf.name "archived-projects-dir"
            ],
        pure defaultArchivedProjectsDirSpec
      ]

defaultArchivedProjectsDirSpec :: ArchivedProjectsDirSpec
defaultArchivedProjectsDirSpec = ArchivedProjectsInArchive [reldir|projects|]
