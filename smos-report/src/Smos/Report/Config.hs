{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Config
  ( SmosReportConfig (..),
    defaultReportConfig,
    DirectorySettings (..),
    defaultDirectorySettings,
    WaitingReportConfig (..),
    defaultWaitingReportConfig,
    defaultWaitingThreshold,
    StuckReportConfig (..),
    defaultStuckReportConfig,
    defaultStuckThreshold,
    WorkReportConfig (..),
    defaultWorkReportConfig,
    defaultWorkBaseFilter,
    defaultProjection,
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
import Smos.Data
import Smos.Directory.Config
import Smos.Report.Filter
import Smos.Report.Projection
import Smos.Report.Sorter
import Smos.Report.Time

data SmosReportConfig = SmosReportConfig
  { smosReportConfigDirectorySettings :: !DirectorySettings,
    smosReportConfigWaitingConfig :: !WaitingReportConfig,
    smosReportConfigStuckConfig :: !StuckReportConfig,
    smosReportConfigWorkConfig :: !WorkReportConfig
  }
  deriving (Show, Eq, Generic)

defaultReportConfig :: SmosReportConfig
defaultReportConfig =
  SmosReportConfig
    { smosReportConfigDirectorySettings = defaultDirectorySettings,
      smosReportConfigWaitingConfig = defaultWaitingReportConfig,
      smosReportConfigStuckConfig = defaultStuckReportConfig,
      smosReportConfigWorkConfig = defaultWorkReportConfig
    }

data WorkReportConfig = WorkReportConfig
  { workReportConfigBaseFilter :: Maybe EntryFilter,
    workReportConfigChecks :: Set EntryFilter,
    workReportConfigContexts :: Map ContextName EntryFilter,
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

defaultWorkBaseFilter :: EntryFilter
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

resolveReportWorkflowDir :: SmosReportConfig -> IO (Path Abs Dir)
resolveReportWorkflowDir = resolveDirWorkflowDir . smosReportConfigDirectorySettings

resolveReportArchiveDir :: SmosReportConfig -> IO (Path Abs Dir)
resolveReportArchiveDir = resolveDirArchiveDir . smosReportConfigDirectorySettings

resolveReportProjectsDir :: SmosReportConfig -> IO (Path Abs Dir)
resolveReportProjectsDir = resolveDirProjectsDir . smosReportConfigDirectorySettings

resolveReportArchivedProjectsDir :: SmosReportConfig -> IO (Path Abs Dir)
resolveReportArchivedProjectsDir = resolveDirArchivedProjectsDir . smosReportConfigDirectorySettings

newtype ContextName = ContextName
  { contextNameText :: Text
  }
  deriving (Show, Eq, Ord, Generic, FromJSONKey, ToJSONKey, FromJSON, ToJSON)

instance Validity ContextName
