{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Report.Config
  ( SmosReportConfig(..)
  , defaultReportConfig
  , defaultWorkflowDirSpec
  , WorkflowDirSpec(..)
  , resolveWorkflowDir
  , resolveArchiveDir
  , resolveReportWorkflowDir
  , resolveReportArchiveDir
  ) where

import GHC.Generics (Generic)

import Path
import Path.IO

data SmosReportConfig =
  SmosReportConfig
    { smosReportConfigAgendaFileSpec :: !WorkflowDirSpec
    , smosReportConfigArchiveFileSpec :: !ArchiveDirSpec
    }
  deriving (Show, Eq, Generic)

defaultReportConfig :: SmosReportConfig
defaultReportConfig =
  SmosReportConfig
    { smosReportConfigAgendaFileSpec = defaultWorkflowDirSpec
    , smosReportConfigArchiveFileSpec = defaultArchiveDirSpec
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
  | ArchiveAbsolute (Path Abs Dir)
  deriving (Show, Eq, Generic)

defaultArchiveDirSpec :: ArchiveDirSpec
defaultArchiveDirSpec = ArchiveInWorkflow [reldir|archive|]

resolveArchiveDir :: Path Abs Dir -> ArchiveDirSpec -> Path Abs Dir
resolveArchiveDir wd as =
  case as of
    ArchiveInWorkflow ard -> wd </> ard
    ArchiveAbsolute aad -> aad

resolveReportWorkflowDir :: SmosReportConfig -> IO (Path Abs Dir)
resolveReportWorkflowDir SmosReportConfig {..} = resolveWorkflowDir smosReportConfigAgendaFileSpec

resolveReportArchiveDir :: SmosReportConfig -> IO (Path Abs Dir)
resolveReportArchiveDir SmosReportConfig {..} =
  resolveArchiveDir <$> resolveWorkflowDir smosReportConfigAgendaFileSpec <*>
  pure smosReportConfigArchiveFileSpec
