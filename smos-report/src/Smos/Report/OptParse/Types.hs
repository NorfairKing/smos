{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.OptParse.Types where

import Autodocodec
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Validity
import GHC.Generics (Generic)
import Smos.Data
import Smos.Directory.OptParse.Types
import Smos.Report.Filter
import Smos.Report.Projection
import Smos.Report.Sorter
import Smos.Report.Time

data Flags = Flags
  { flagDirectoryFlags :: DirectoryFlags
  }
  deriving (Show, Eq, Generic)

data Environment = Environment
  { envDirectoryEnvironment :: DirectoryEnvironment
  }
  deriving (Show, Eq, Generic)

emptyEnvironment :: Environment
emptyEnvironment =
  Environment
    { envDirectoryEnvironment = emptyDirectoryEnvironment
    }

data Configuration = Configuration
  { confDirectoryConf :: !DirectoryConfiguration,
    confWaitingReportConf :: !(Maybe WaitingReportConfiguration),
    confStuckReportConf :: !(Maybe StuckReportConfiguration),
    confWorkReportConf :: !(Maybe WorkReportConfiguration)
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via (Autodocodec Configuration)

instance Validity Configuration

instance HasCodec Configuration where
  codec = object "Configuration" objectCodec

instance HasObjectCodec Configuration where
  objectCodec =
    Configuration
      <$> objectCodec .= confDirectoryConf
      <*> optionalFieldOrNull "waiting" "The waiting report configuration" .= confWaitingReportConf
      <*> optionalFieldOrNull "stuck" "The stuck projects report configuration" .= confStuckReportConf
      <*> optionalFieldOrNull "work" "The work report configuration" .= confWorkReportConf

defaultConfiguration :: Configuration
defaultConfiguration =
  Configuration
    { confDirectoryConf = defaultDirectoryConfiguration,
      confWorkReportConf = Nothing,
      confStuckReportConf = Nothing,
      confWaitingReportConf = Nothing
    }

backToConfiguration :: SmosReportSettings -> Configuration
backToConfiguration SmosReportSettings {..} =
  Configuration
    { confDirectoryConf = backToDirectoryConfiguration smosReportSettingDirectorySettings,
      confWaitingReportConf =
        if smosReportSettingWaitingSettings == defaultWaitingReportSettings
          then Nothing
          else Just $ backToWaitingReportConfiguration smosReportSettingWaitingSettings,
      confStuckReportConf =
        if smosReportSettingStuckSettings == defaultStuckReportSettings
          then Nothing
          else Just $ backToStuckReportConfiguration smosReportSettingStuckSettings,
      confWorkReportConf =
        if smosReportSettingWorkSettings == defaultWorkReportSettings
          then Nothing
          else Just $ backToWorkReportConfiguration smosReportSettingWorkSettings
    }

data WaitingReportConfiguration = WaitingReportConfiguration
  { waitingReportConfThreshold :: !(Maybe Time)
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via (Autodocodec WaitingReportConfiguration)

instance Validity WaitingReportConfiguration

instance HasCodec WaitingReportConfiguration where
  codec =
    object "WaitingReportConfiguration" $
      WaitingReportConfiguration
        <$> optionalFieldOrNull "threshold" "waiting report threshold to consider waiting entries 'overdue'" .= waitingReportConfThreshold

backToWaitingReportConfiguration :: WaitingReportSettings -> WaitingReportConfiguration
backToWaitingReportConfiguration WaitingReportSettings {..} =
  WaitingReportConfiguration
    { waitingReportConfThreshold =
        if waitingReportSettingThreshold == defaultWaitingThreshold
          then Nothing
          else Just defaultWaitingThreshold
    }

data StuckReportConfiguration = StuckReportConfiguration
  { stuckReportConfThreshold :: !(Maybe Time)
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via (Autodocodec StuckReportConfiguration)

instance Validity StuckReportConfiguration

instance HasCodec StuckReportConfiguration where
  codec =
    object "StuckReportConfiguration" $
      StuckReportConfiguration
        <$> optionalFieldOrNull "threshold" "stuck report threshold to consider stuck projects 'overdue'" .= stuckReportConfThreshold

backToStuckReportConfiguration :: StuckReportSettings -> StuckReportConfiguration
backToStuckReportConfiguration StuckReportSettings {..} =
  StuckReportConfiguration
    { stuckReportConfThreshold =
        if stuckReportSettingThreshold == defaultStuckThreshold
          then Nothing
          else Just defaultStuckThreshold
    }

newtype ContextName = ContextName
  { contextNameText :: Text
  }
  deriving (Show, Eq, Ord, Generic, FromJSONKey, ToJSONKey, FromJSON, ToJSON)

instance Validity ContextName

data WorkReportConfiguration = WorkReportConfiguration
  { workReportConfBaseFilter :: !(Maybe EntryFilter),
    workReportConfChecks :: !(Maybe (Set EntryFilter)),
    workReportConfContexts :: !(Maybe (Map ContextName EntryFilter)),
    workReportConfTimeFilterProperty :: Maybe PropertyName,
    workReportConfProjection :: Maybe (NonEmpty Projection),
    workReportConfSorter :: Maybe Sorter
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec WorkReportConfiguration)

instance Validity WorkReportConfiguration

instance HasCodec WorkReportConfiguration where
  codec =
    object "WorkReportConfiguration" $
      WorkReportConfiguration
        <$> optionalFieldOrNull "base-filter" "The base work filter" .= workReportConfBaseFilter
        <*> optionalFieldOrNull "checks" "Checks for the work report" .= workReportConfChecks
        <*> optionalFieldOrNull "contexts" "Contexts for the work report" .= workReportConfContexts
        <*> optionalFieldOrNull "time-filter" "The property to use to filter by time" .= workReportConfTimeFilterProperty
        <*> optionalFieldOrNull "columns" "The columns in the report" .= workReportConfProjection
        <*> optionalFieldOrNull "sorter" "The sorter to use to sort the rows" .= workReportConfSorter

defaultWorkReportConfiguration :: WorkReportConfiguration
defaultWorkReportConfiguration =
  WorkReportConfiguration
    { workReportConfBaseFilter = Nothing,
      workReportConfChecks = Nothing,
      workReportConfContexts = Nothing,
      workReportConfTimeFilterProperty = Nothing,
      workReportConfProjection = Nothing,
      workReportConfSorter = Nothing
    }

backToWorkReportConfiguration :: WorkReportSettings -> WorkReportConfiguration
backToWorkReportConfiguration WorkReportSettings {..} =
  WorkReportConfiguration
    { workReportConfBaseFilter =
        if workReportSettingBaseFilter == Just defaultWorkBaseFilter
          then Nothing
          else Just defaultWorkBaseFilter,
      workReportConfChecks = Just workReportSettingChecks,
      workReportConfContexts = Just workReportSettingContexts,
      workReportConfTimeFilterProperty = workReportSettingTimeProperty,
      workReportConfProjection = Just workReportSettingProjection,
      workReportConfSorter = workReportSettingSorter
    }

data SmosReportSettings = SmosReportSettings
  { smosReportSettingDirectorySettings :: !DirectorySettings,
    smosReportSettingWaitingSettings :: !WaitingReportSettings,
    smosReportSettingStuckSettings :: !StuckReportSettings,
    smosReportSettingWorkSettings :: !WorkReportSettings
  }
  deriving (Show, Eq, Generic)

instance Validity SmosReportSettings

defaultSmosReportSettings :: SmosReportSettings
defaultSmosReportSettings =
  SmosReportSettings
    { smosReportSettingDirectorySettings = defaultDirectorySettings,
      smosReportSettingWaitingSettings = defaultWaitingReportSettings,
      smosReportSettingStuckSettings = defaultStuckReportSettings,
      smosReportSettingWorkSettings = defaultWorkReportSettings
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

instance Validity WorkReportSettings

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

instance Validity WaitingReportSettings

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

instance Validity StuckReportSettings

defaultStuckReportSettings :: StuckReportSettings
defaultStuckReportSettings =
  StuckReportSettings
    { stuckReportSettingThreshold = defaultStuckThreshold
    }

defaultStuckThreshold :: Time
defaultStuckThreshold = Weeks 3
