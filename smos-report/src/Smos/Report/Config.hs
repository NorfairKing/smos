{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Config
  ( SmosReportConfig (..),
    defaultReportConfig,
    DirectoryConfig (..),
    defaultDirectoryConfig,
    WaitingReportConfig (..),
    defaultWaitingReportConfig,
    defaultWaitingThreshold,
    StuckReportConfig (..),
    defaultStuckReportConfig,
    defaultStuckThreshold,
    WorkReportConfig (..),
    defaultWorkReportConfig,
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
    defaultProjection,
    defaultArchivedProjectsDirSpec,
    resolveArchivedProjectsDir,
    resolveDirWorkflowDir,
    resolveDirArchiveDir,
    resolveDirProjectsDir,
    resolveDirArchivedProjectsDir,
    resolveReportWorkflowDir,
    resolveReportArchiveDir,
    resolveReportProjectsDir,
    resolveReportArchivedProjectsDir,
    ContextName (..),
  )
where

import Data.Aeson
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Validity
import GHC.Generics (Generic)
import Path
import Path.IO
import Smos.Data
import Smos.Report.Filter
import Smos.Report.Projection
import Smos.Report.Sorter
import Smos.Report.Time
import YamlParse.Applicative

data SmosReportConfig = SmosReportConfig
  { smosReportConfigDirectoryConfig :: !DirectoryConfig,
    smosReportConfigWaitingConfig :: !WaitingReportConfig,
    smosReportConfigStuckConfig :: !StuckReportConfig,
    smosReportConfigWorkConfig :: !WorkReportConfig
  }
  deriving (Show, Eq, Generic)

defaultReportConfig :: SmosReportConfig
defaultReportConfig =
  SmosReportConfig
    { smosReportConfigDirectoryConfig = defaultDirectoryConfig,
      smosReportConfigWaitingConfig = defaultWaitingReportConfig,
      smosReportConfigStuckConfig = defaultStuckReportConfig,
      smosReportConfigWorkConfig = defaultWorkReportConfig
    }

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

data WorkReportConfig = WorkReportConfig
  { workReportConfigBaseFilter :: Maybe EntryFilterRel,
    workReportConfigChecks :: Set EntryFilterRel,
    workReportConfigContexts :: Map ContextName EntryFilterRel,
    workReportConfigTimeProperty :: Maybe PropertyName,
    workReportConfigProjection :: NonEmpty Projection,
    workReportConfigSorter :: Maybe Sorter
  }
  deriving (Show, Eq, Generic)

defaultWorkReportConfig :: WorkReportConfig
defaultWorkReportConfig =
  WorkReportConfig
    { workReportConfigBaseFilter = Just defaultWorkBaseFilter,
      workReportConfigChecks = S.empty,
      workReportConfigContexts = M.empty,
      workReportConfigTimeProperty = Nothing,
      workReportConfigProjection = defaultProjection,
      workReportConfigSorter = Nothing
    }

defaultProjection :: NonEmpty Projection
defaultProjection = OntoFile :| [OntoState, OntoHeader]

defaultWorkBaseFilter :: EntryFilterRel
defaultWorkBaseFilter =
  FilterSnd $
    FilterWithinCursor $
      FilterEntryTodoState $
        FilterMaybe False $
          FilterOr (FilterSub "NEXT") (FilterSub "STARTED")

data WaitingReportConfig = WaitingReportConfig
  { waitingReportConfigThreshold :: Time
  }
  deriving (Show, Eq, Generic)

defaultWaitingReportConfig :: WaitingReportConfig
defaultWaitingReportConfig =
  WaitingReportConfig
    { waitingReportConfigThreshold = defaultWaitingThreshold
    }

defaultWaitingThreshold :: Time
defaultWaitingThreshold = Days 7

data StuckReportConfig = StuckReportConfig
  { stuckReportConfigThreshold :: Time
  }
  deriving (Show, Eq, Generic)

defaultStuckReportConfig :: StuckReportConfig
defaultStuckReportConfig =
  StuckReportConfig
    { stuckReportConfigThreshold = defaultStuckThreshold
    }

defaultStuckThreshold :: Time
defaultStuckThreshold = Weeks 3

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

resolveReportWorkflowDir :: SmosReportConfig -> IO (Path Abs Dir)
resolveReportWorkflowDir = resolveDirWorkflowDir . smosReportConfigDirectoryConfig

resolveReportArchiveDir :: SmosReportConfig -> IO (Path Abs Dir)
resolveReportArchiveDir = resolveDirArchiveDir . smosReportConfigDirectoryConfig

resolveReportProjectsDir :: SmosReportConfig -> IO (Path Abs Dir)
resolveReportProjectsDir = resolveDirProjectsDir . smosReportConfigDirectoryConfig

resolveReportArchivedProjectsDir :: SmosReportConfig -> IO (Path Abs Dir)
resolveReportArchivedProjectsDir = resolveDirArchivedProjectsDir . smosReportConfigDirectoryConfig

newtype ContextName = ContextName
  { contextNameText :: Text
  }
  deriving (Show, Eq, Ord, Generic, FromJSONKey, ToJSONKey, FromJSON, ToJSON)

instance Validity ContextName

instance YamlKeySchema ContextName where
  yamlKeySchema = ContextName <$> yamlKeySchema
