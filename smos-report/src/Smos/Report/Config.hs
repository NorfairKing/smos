{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Report.Config
  ( SmosReportConfig(..)
  , defaultReportConfig
  , WorkflowDirSpec(..)
  , defaultWorkflowDirSpec
  , resolveWorkflowDir
  , ArchiveDirSpec(..)
  , defaultArchiveDirSpec
  , resolveArchiveDir
  , ProjectsDirSpec(..)
  , defaultProjectsDirSpec
  , resolveProjectsDir
  , resolveReportWorkflowDir
  , resolveReportArchiveDir
  , resolveReportProjectsDir
  ) where

import GHC.Generics (Generic)

import Path
import Path.IO

data SmosReportConfig =
  SmosReportConfig
    { smosReportConfigWorkflowFileSpec :: !WorkflowDirSpec
    , smosReportConfigArchiveFileSpec :: !ArchiveDirSpec
    , smosReportConfigProjectsFileSpec :: !ProjectsDirSpec
    }
  deriving (Show, Eq, Generic)

defaultReportConfig :: SmosReportConfig
defaultReportConfig =
  SmosReportConfig
    { smosReportConfigWorkflowFileSpec = defaultWorkflowDirSpec
    , smosReportConfigArchiveFileSpec = defaultArchiveDirSpec
    , smosReportConfigProjectsFileSpec = defaultProjectsDirSpec
    }

data WorkflowDirSpec
  = DirInHome (Path Rel Dir)
  | DirAbsolute (Path Abs Dir)
  deriving (Show, Eq, Generic)

defaultWorkflowDirSpec :: WorkflowDirSpec
defaultWorkflowDirSpec = DirInHome [reldir|workflow|]

resolveWorkflowDir :: WorkflowDirSpec -> IO (Path Abs Dir)
resolveWorkflowDir afs =
  case afs of
    DirInHome rp -> getHomeDir >>= (`resolveDir` fromRelDir rp)
    DirAbsolute ad -> pure ad

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

resolveReportWorkflowDir :: SmosReportConfig -> IO (Path Abs Dir)
resolveReportWorkflowDir SmosReportConfig {..} = resolveWorkflowDir smosReportConfigWorkflowFileSpec

resolveReportArchiveDir :: SmosReportConfig -> IO (Path Abs Dir)
resolveReportArchiveDir SmosReportConfig {..} = do
  wd <- resolveWorkflowDir smosReportConfigWorkflowFileSpec
  resolveArchiveDir wd smosReportConfigArchiveFileSpec

resolveReportProjectsDir :: SmosReportConfig -> IO (Path Abs Dir)
resolveReportProjectsDir SmosReportConfig {..} = do
  wd <- resolveWorkflowDir smosReportConfigWorkflowFileSpec
  resolveProjectsDir wd smosReportConfigProjectsFileSpec
