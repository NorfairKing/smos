{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Directory.Config
  ( DirectoryConfig (..),
    defaultDirectoryConfig,
    WorkflowDirSpec (..),
    defaultWorkflowDirSpec,
    resolveWorkflowDir,
    ArchiveDirSpec (..),
    defaultArchiveDirSpec,
    resolveArchiveDir,
    ProjectsDirSpec (..),
    defaultProjectsDirSpec,
    resolveProjectsDir,
    ArchivedProjectsDirSpec (..),
    defaultArchivedProjectsDirSpec,
    resolveArchivedProjectsDir,
    resolveDirWorkflowDir,
    resolveDirArchiveDir,
    resolveDirProjectsDir,
    resolveDirArchivedProjectsDir,
  )
where

import GHC.Generics (Generic)
import Path
import Path.IO

data DirectoryConfig = DirectoryConfig
  { directoryConfigWorkflowFileSpec :: !WorkflowDirSpec,
    directoryConfigArchiveFileSpec :: !ArchiveDirSpec,
    directoryConfigProjectsFileSpec :: !ProjectsDirSpec,
    directoryConfigArchivedProjectsFileSpec :: !ArchivedProjectsDirSpec
  }
  deriving (Show, Eq, Generic)

defaultDirectoryConfig :: DirectoryConfig
defaultDirectoryConfig =
  DirectoryConfig
    { directoryConfigWorkflowFileSpec = defaultWorkflowDirSpec,
      directoryConfigArchiveFileSpec = defaultArchiveDirSpec,
      directoryConfigProjectsFileSpec = defaultProjectsDirSpec,
      directoryConfigArchivedProjectsFileSpec = defaultArchivedProjectsDirSpec
    }

data WorkflowDirSpec
  = WorkflowInHome (Path Rel Dir)
  | AbsoluteWorkflow (Path Abs Dir)
  deriving (Show, Eq, Generic)

defaultWorkflowDirSpec :: WorkflowDirSpec
defaultWorkflowDirSpec = WorkflowInHome [reldir|workflow|]

resolveWorkflowDir :: WorkflowDirSpec -> IO (Path Abs Dir)
resolveWorkflowDir afs =
  case afs of
    WorkflowInHome rp -> getHomeDir >>= (`resolveDir` fromRelDir rp)
    AbsoluteWorkflow ad -> pure ad

data ArchiveDirSpec
  = ArchiveInWorkflow (Path Rel Dir)
  | ArchiveInHome (Path Rel Dir)
  | ArchiveAbsolute (Path Abs Dir)
  deriving (Show, Eq, Generic)

defaultArchiveDirSpec :: ArchiveDirSpec
defaultArchiveDirSpec = ArchiveInWorkflow [reldir|archive|]

resolveArchiveDir :: Path Abs Dir -> ArchiveDirSpec -> IO (Path Abs Dir)
resolveArchiveDir wd as =
  case as of
    ArchiveInWorkflow ard -> pure $ wd </> ard
    ArchiveInHome ard -> (</> ard) <$> getHomeDir
    ArchiveAbsolute aad -> pure aad

data ProjectsDirSpec
  = ProjectsInWorkflow (Path Rel Dir)
  | ProjectsInHome (Path Rel Dir)
  | ProjectsAbsolute (Path Abs Dir)
  deriving (Show, Eq, Generic)

defaultProjectsDirSpec :: ProjectsDirSpec
defaultProjectsDirSpec = ProjectsInWorkflow [reldir|projects|]

resolveProjectsDir :: Path Abs Dir -> ProjectsDirSpec -> IO (Path Abs Dir)
resolveProjectsDir wd as =
  case as of
    ProjectsInWorkflow ard -> pure $ wd </> ard
    ProjectsInHome ard -> (</> ard) <$> getHomeDir
    ProjectsAbsolute aad -> pure aad

data ArchivedProjectsDirSpec
  = ArchivedProjectsInArchive (Path Rel Dir)
  | ArchivedProjectsInHome (Path Rel Dir)
  | ArchivedProjectsAbsolute (Path Abs Dir)
  deriving (Show, Eq, Generic)

defaultArchivedProjectsDirSpec :: ArchivedProjectsDirSpec
defaultArchivedProjectsDirSpec = ArchivedProjectsInArchive [reldir|projects|]

resolveArchivedProjectsDir :: Path Abs Dir -> ArchivedProjectsDirSpec -> IO (Path Abs Dir)
resolveArchivedProjectsDir ad as =
  case as of
    ArchivedProjectsInArchive ard -> pure $ ad </> ard
    ArchivedProjectsInHome ard -> (</> ard) <$> getHomeDir
    ArchivedProjectsAbsolute aad -> pure aad

resolveDirWorkflowDir :: DirectoryConfig -> IO (Path Abs Dir)
resolveDirWorkflowDir DirectoryConfig {..} = resolveWorkflowDir directoryConfigWorkflowFileSpec

resolveDirArchiveDir :: DirectoryConfig -> IO (Path Abs Dir)
resolveDirArchiveDir DirectoryConfig {..} = do
  wd <- resolveWorkflowDir directoryConfigWorkflowFileSpec
  resolveArchiveDir wd directoryConfigArchiveFileSpec

resolveDirProjectsDir :: DirectoryConfig -> IO (Path Abs Dir)
resolveDirProjectsDir DirectoryConfig {..} = do
  wd <- resolveWorkflowDir directoryConfigWorkflowFileSpec
  resolveProjectsDir wd directoryConfigProjectsFileSpec

resolveDirArchivedProjectsDir :: DirectoryConfig -> IO (Path Abs Dir)
resolveDirArchivedProjectsDir DirectoryConfig {..} = do
  wd <- resolveWorkflowDir directoryConfigWorkflowFileSpec
  ad <- resolveArchiveDir wd directoryConfigArchiveFileSpec
  resolveArchivedProjectsDir ad directoryConfigArchivedProjectsFileSpec
