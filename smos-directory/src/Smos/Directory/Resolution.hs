{-# LANGUAGE RecordWildCards #-}

module Smos.Directory.Resolution where

import Path
import Path.IO
import Smos.Directory.OptParse

resolveWorkflowDir :: WorkflowDirSpec -> IO (Path Abs Dir)
resolveWorkflowDir afs =
  case afs of
    WorkflowInHome rp -> getHomeDir >>= (`resolveDir` fromRelDir rp)
    AbsoluteWorkflow ad -> pure ad

resolveArchiveDir :: Path Abs Dir -> ArchiveDirSpec -> IO (Path Abs Dir)
resolveArchiveDir wd as =
  case as of
    ArchiveInWorkflow ard -> pure $ wd </> ard
    ArchiveInHome ard -> (</> ard) <$> getHomeDir
    ArchiveAbsolute aad -> pure aad

resolveProjectsDir :: Path Abs Dir -> ProjectsDirSpec -> IO (Path Abs Dir)
resolveProjectsDir wd as =
  case as of
    ProjectsInWorkflow ard -> pure $ wd </> ard
    ProjectsInHome ard -> (</> ard) <$> getHomeDir
    ProjectsAbsolute aad -> pure aad

resolveArchivedProjectsDir :: Path Abs Dir -> ArchivedProjectsDirSpec -> IO (Path Abs Dir)
resolveArchivedProjectsDir ad as =
  case as of
    ArchivedProjectsInArchive ard -> pure $ ad </> ard
    ArchivedProjectsInHome ard -> (</> ard) <$> getHomeDir
    ArchivedProjectsAbsolute aad -> pure aad

resolveDirWorkflowDir :: DirectorySettings -> IO (Path Abs Dir)
resolveDirWorkflowDir DirectorySettings {..} = resolveWorkflowDir directoryConfigWorkflowFileSpec

resolveDirArchiveDir :: DirectorySettings -> IO (Path Abs Dir)
resolveDirArchiveDir DirectorySettings {..} = do
  wd <- resolveWorkflowDir directoryConfigWorkflowFileSpec
  resolveArchiveDir wd directoryConfigArchiveFileSpec

resolveDirProjectsDir :: DirectorySettings -> IO (Path Abs Dir)
resolveDirProjectsDir DirectorySettings {..} = do
  wd <- resolveWorkflowDir directoryConfigWorkflowFileSpec
  resolveProjectsDir wd directoryConfigProjectsFileSpec

resolveDirArchivedProjectsDir :: DirectorySettings -> IO (Path Abs Dir)
resolveDirArchivedProjectsDir DirectorySettings {..} = do
  wd <- resolveWorkflowDir directoryConfigWorkflowFileSpec
  ad <- resolveArchiveDir wd directoryConfigArchiveFileSpec
  resolveArchivedProjectsDir ad directoryConfigArchivedProjectsFileSpec
