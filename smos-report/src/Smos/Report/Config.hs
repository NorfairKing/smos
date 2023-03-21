{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.Config
  ( SmosReportSettings (..),
    defaultReportConfig,
    DirectorySettings (..),
    defaultDirectorySettings,
    WaitingReportSettings (..),
    defaultWaitingReportSettings,
    defaultWaitingThreshold,
    StuckReportSettings (..),
    defaultStuckReportSettings,
    defaultStuckThreshold,
    WorkReportSettings (..),
    defaultWorkReportSettings,
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

data SmosReportSettings = SmosReportSettings
  { smosReportSettingDirectorySettings :: !DirectorySettings,
    smosReportSettingWaitingConfig :: !WaitingReportSettings,
    smosReportSettingStuckConfig :: !StuckReportSettings,
    smosReportSettingWorkConfig :: !WorkReportSettings
  }
  deriving (Show, Eq, Generic)

defaultReportConfig :: SmosReportSettings
defaultReportConfig =
  SmosReportSettings
    { smosReportSettingDirectorySettings = defaultDirectorySettings,
      smosReportSettingWaitingConfig = defaultWaitingReportSettings,
      smosReportSettingStuckConfig = defaultStuckReportSettings,
      smosReportSettingWorkConfig = defaultWorkReportSettings
    }

data WorkReportSettings = WorkReportSettings
  { workReportSettingBaseFilter :: Maybe EntryFilter,
    workReportSettingChecks :: Set EntryFilter,
    workReportSettingContexts :: Map ContextName EntryFilter,
    workReportSettingTimeProperty :: Maybe PropertyName,
    workReportSettingProjection :: NonEmpty Projection,
    workReportSettingSorter :: Maybe Sorter
  }
  deriving (Show, Eq, Generic)

defaultWorkReportSettings :: WorkReportSettings
defaultWorkReportSettings =
  WorkReportSettings
    { workReportSettingBaseFilter = Just defaultWorkBaseFilter,
      workReportSettingChecks = S.empty,
      workReportSettingContexts = M.empty,
      workReportSettingTimeProperty = Nothing,
      workReportSettingProjection = defaultProjection,
      workReportSettingSorter = Nothing
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

data WaitingReportSettings = WaitingReportSettings
  { waitingReportSettingThreshold :: Time
  }
  deriving (Show, Eq, Generic)

defaultWaitingReportSettings :: WaitingReportSettings
defaultWaitingReportSettings =
  WaitingReportSettings
    { waitingReportSettingThreshold = defaultWaitingThreshold
    }

defaultWaitingThreshold :: Time
defaultWaitingThreshold = Days 7

data StuckReportSettings = StuckReportSettings
  { stuckReportSettingThreshold :: Time
  }
  deriving (Show, Eq, Generic)

defaultStuckReportSettings :: StuckReportSettings
defaultStuckReportSettings =
  StuckReportSettings
    { stuckReportSettingThreshold = defaultStuckThreshold
    }

defaultStuckThreshold :: Time
defaultStuckThreshold = Weeks 3

resolveReportWorkflowDir :: SmosReportSettings -> IO (Path Abs Dir)
resolveReportWorkflowDir = resolveDirWorkflowDir . smosReportSettingDirectorySettings

resolveReportArchiveDir :: SmosReportSettings -> IO (Path Abs Dir)
resolveReportArchiveDir = resolveDirArchiveDir . smosReportSettingDirectorySettings

resolveReportProjectsDir :: SmosReportSettings -> IO (Path Abs Dir)
resolveReportProjectsDir = resolveDirProjectsDir . smosReportSettingDirectorySettings

resolveReportArchivedProjectsDir :: SmosReportSettings -> IO (Path Abs Dir)
resolveReportArchivedProjectsDir = resolveDirArchivedProjectsDir . smosReportSettingDirectorySettings

newtype ContextName = ContextName
  { contextNameText :: Text
  }
  deriving (Show, Eq, Ord, Generic, FromJSONKey, ToJSONKey, FromJSON, ToJSON)

instance Validity ContextName
