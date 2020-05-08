{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Config
  ( SmosReportConfig (..),
    defaultReportConfig,
    defaultWorkBaseFilter,
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
    resolveReportWorkflowDir,
    resolveReportArchiveDir,
    resolveReportProjectsDir,
    resolveReportArchivedProjectsDir,
    ContextName (..),
  )
where

import Data.Aeson
import qualified Data.Map as M
import Data.Map (Map)
import Data.Text (Text)
import Data.Validity
import GHC.Generics (Generic)
import Path
import Path.IO
import Smos.Report.Filter
import YamlParse.Applicative

data SmosReportConfig
  = SmosReportConfig
      { smosReportConfigWorkflowFileSpec :: !WorkflowDirSpec,
        smosReportConfigArchiveFileSpec :: !ArchiveDirSpec,
        smosReportConfigProjectsFileSpec :: !ProjectsDirSpec,
        smosReportConfigArchivedProjectsFileSpec :: !ArchivedProjectsDirSpec,
        smosReportConfigWorkBaseFilter :: Maybe EntryFilter,
        smosReportConfigContexts :: Map ContextName EntryFilter
      }
  deriving (Show, Eq, Generic)

defaultReportConfig :: SmosReportConfig
defaultReportConfig =
  SmosReportConfig
    { smosReportConfigWorkflowFileSpec = defaultWorkflowDirSpec,
      smosReportConfigArchiveFileSpec = defaultArchiveDirSpec,
      smosReportConfigProjectsFileSpec = defaultProjectsDirSpec,
      smosReportConfigArchivedProjectsFileSpec = defaultArchivedProjectsDirSpec,
      smosReportConfigWorkBaseFilter = Just defaultWorkBaseFilter,
      smosReportConfigContexts = M.fromList []
    }

defaultWorkBaseFilter :: EntryFilter
defaultWorkBaseFilter =
  FilterSnd
    $ FilterWithinCursor
    $ FilterEntryTodoState
    $ FilterMaybe False
    $ FilterOr (FilterSub "NEXT") (FilterSub "STARTED")

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

resolveReportArchivedProjectsDir :: SmosReportConfig -> IO (Path Abs Dir)
resolveReportArchivedProjectsDir SmosReportConfig {..} = do
  wd <- resolveWorkflowDir smosReportConfigWorkflowFileSpec
  ad <- resolveArchiveDir wd smosReportConfigArchiveFileSpec
  resolveArchivedProjectsDir ad smosReportConfigArchivedProjectsFileSpec

newtype ContextName
  = ContextName
      { contextNameText :: Text
      }
  deriving (Show, Eq, Ord, Generic, FromJSONKey, ToJSONKey, FromJSON, ToJSON)

instance Validity ContextName

instance YamlKeySchema ContextName where
  yamlKeySchema = ContextName <$> yamlKeySchema
