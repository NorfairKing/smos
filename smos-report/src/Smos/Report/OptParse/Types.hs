{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Report.OptParse.Types where

import Autodocodec
import Data.Aeson (FromJSONKey, ToJSONKey)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Time
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

data Environment = Environment
  { envDirectoryEnvironment :: DirectoryEnvironment
  }

data Configuration = Configuration
  { confDirectoryConf :: !DirectoryConfiguration,
    confWaitingReportConf :: !(Maybe WaitingReportConfiguration),
    confStuckReportConf :: !(Maybe StuckReportConfiguration),
    confWorkReportConf :: !(Maybe WorkReportConfiguration),
    confFreeReportConf :: !(Maybe FreeReportConfiguration)
  }
  deriving stock (Show, Generic)

instance Validity Configuration

instance HasObjectCodec Configuration where
  objectCodec =
    Configuration
      <$> objectCodec
        .= confDirectoryConf
      <*> optionalFieldOrNull "waiting" "The waiting report configuration"
        .= confWaitingReportConf
      <*> optionalFieldOrNull "stuck" "The stuck projects report configuration"
        .= confStuckReportConf
      <*> optionalFieldOrNull "work" "The work report configuration"
        .= confWorkReportConf
      <*> optionalFieldOrNull "free" "The free report configuration"
        .= confFreeReportConf

defaultConfiguration :: Configuration
defaultConfiguration =
  Configuration
    { confDirectoryConf = defaultDirectoryConfiguration,
      confWaitingReportConf = Nothing,
      confStuckReportConf = Nothing,
      confWorkReportConf = Nothing,
      confFreeReportConf = Nothing
    }

data WaitingReportConfiguration = WaitingReportConfiguration
  { waitingReportConfThreshold :: !(Maybe Time)
  }
  deriving stock (Show, Generic)

instance Validity WaitingReportConfiguration

instance HasCodec WaitingReportConfiguration where
  codec =
    object "WaitingReportConfiguration" $
      WaitingReportConfiguration
        <$> optionalFieldOrNull "threshold" "waiting report threshold to consider waiting entries 'overdue'"
          .= waitingReportConfThreshold

data StuckReportConfiguration = StuckReportConfiguration
  { stuckReportConfThreshold :: !(Maybe Time)
  }
  deriving stock (Show, Generic)

instance Validity StuckReportConfiguration

instance HasCodec StuckReportConfiguration where
  codec =
    object "StuckReportConfiguration" $
      StuckReportConfiguration
        <$> optionalFieldOrNull "threshold" "stuck report threshold to consider stuck projects 'overdue'"
          .= stuckReportConfThreshold

newtype ContextName = ContextName
  { contextNameText :: Text
  }
  deriving (Show, Eq, Ord, Generic, FromJSONKey, ToJSONKey)

instance Validity ContextName

data WorkReportConfiguration = WorkReportConfiguration
  { workReportConfBaseFilter :: !(Maybe EntryFilter),
    workReportConfChecks :: !(Maybe (Set EntryFilter)),
    workReportConfContexts :: !(Maybe (Map ContextName EntryFilter)),
    workReportConfTimeFilterProperty :: Maybe PropertyName,
    workReportConfProjection :: Maybe (NonEmpty Projection),
    workReportConfSorter :: Maybe Sorter
  }
  deriving stock (Show, Generic)

instance Validity WorkReportConfiguration

instance HasCodec WorkReportConfiguration where
  codec =
    object "WorkReportConfiguration" $
      WorkReportConfiguration
        <$> optionalFieldOrNull "base-filter" "The base work filter"
          .= workReportConfBaseFilter
        <*> optionalFieldOrNull "checks" "Checks for the work report"
          .= workReportConfChecks
        <*> optionalFieldOrNull "contexts" "Contexts for the work report"
          .= workReportConfContexts
        <*> optionalFieldOrNull "time-filter" "The property to use to filter by time"
          .= workReportConfTimeFilterProperty
        <*> optionalFieldOrNull "columns" "The columns in the report"
          .= workReportConfProjection
        <*> optionalFieldOrNull "sorter" "The sorter to use to sort the rows"
          .= workReportConfSorter

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

data FreeReportConfiguration = FreeReportConfiguration
  { freeReportConfigurationEarliestTimeOfDay :: !(Maybe TimeOfDay),
    freeReportConfigurationLatestTimeOfDay :: !(Maybe TimeOfDay)
  }
  deriving (Show, Generic)

instance Validity FreeReportConfiguration

instance HasCodec FreeReportConfiguration where
  codec = object "Configuration" objectCodec

instance HasObjectCodec FreeReportConfiguration where
  objectCodec =
    FreeReportConfiguration
      <$> optionalField "earliest" "the earliest time of day to consider free"
        .= freeReportConfigurationEarliestTimeOfDay
      <*> optionalField "latest" "the latest time of day to consider free"
        .= freeReportConfigurationLatestTimeOfDay

data ReportSettings = ReportSettings
  { reportSettingDirectorySettings :: !DirectorySettings,
    reportSettingWaitingSettings :: !WaitingReportSettings,
    reportSettingStuckSettings :: !StuckReportSettings,
    reportSettingWorkSettings :: !WorkReportSettings,
    reportSettingFreeSettings :: !FreeReportSettings
  }

defaultReportSettings :: ReportSettings
defaultReportSettings =
  ReportSettings
    { reportSettingDirectorySettings = defaultDirectorySettings,
      reportSettingWaitingSettings = defaultWaitingReportSettings,
      reportSettingStuckSettings = defaultStuckReportSettings,
      reportSettingWorkSettings = defaultWorkReportSettings,
      reportSettingFreeSettings = defaultFreeReportSettings
    }

data WaitingReportSettings = WaitingReportSettings
  { waitingReportSettingThreshold :: Time
  }

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

defaultStuckReportSettings :: StuckReportSettings
defaultStuckReportSettings =
  StuckReportSettings
    { stuckReportSettingThreshold = defaultStuckThreshold
    }

defaultStuckThreshold :: Time
defaultStuckThreshold = Weeks 3

data WorkReportSettings = WorkReportSettings
  { workReportSettingBaseFilter :: Maybe EntryFilter,
    workReportSettingChecks :: Set EntryFilter,
    workReportSettingContexts :: Map ContextName EntryFilter,
    workReportSettingTimeProperty :: Maybe PropertyName,
    workReportSettingProjection :: NonEmpty Projection,
    workReportSettingSorter :: Maybe Sorter
  }

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

data FreeReportSettings = FreeReportSettings
  { freeReportSettingEarliestTimeOfDay :: !(Maybe TimeOfDay),
    freeReportSettingLatestTimeOfDay :: !(Maybe TimeOfDay)
  }

defaultFreeReportSettings :: FreeReportSettings
defaultFreeReportSettings =
  FreeReportSettings
    { freeReportSettingEarliestTimeOfDay = Just defaultEarliestFreeTimeOfDay,
      freeReportSettingLatestTimeOfDay = Just defaultLatestFreeTimeOfDay
    }

defaultEarliestFreeTimeOfDay :: TimeOfDay
defaultEarliestFreeTimeOfDay = TimeOfDay 08 00 00

defaultLatestFreeTimeOfDay :: TimeOfDay
defaultLatestFreeTimeOfDay = TimeOfDay 22 00 00
