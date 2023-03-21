{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.OptParse.Types where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Set (Set)
import Data.Validity
import GHC.Generics (Generic)
import Smos.Data
import Smos.Directory.OptParse.Types
import Smos.Report.Config
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
  { confDirectoryConf :: !DirectorySettingsuration,
    confWaitingReportConf :: !(Maybe WaitingReportSettingsuration),
    confStuckReportConf :: !(Maybe StuckReportSettingsuration),
    confWorkReportConf :: !(Maybe WorkReportSettingsuration)
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
    { confDirectoryConf = defaultDirectorySettingsuration,
      confWorkReportConf = Nothing,
      confStuckReportConf = Nothing,
      confWaitingReportConf = Nothing
    }

backToConfiguration :: SmosReportSettings -> Configuration
backToConfiguration SmosReportSettings {..} =
  Configuration
    { confDirectoryConf = backToDirectorySettingsuration smosReportSettingDirectorySettings,
      confWaitingReportConf =
        if smosReportSettingWaitingConfig == defaultWaitingReportSettings
          then Nothing
          else Just $ backToWaitingReportSettingsuration smosReportSettingWaitingConfig,
      confStuckReportConf =
        if smosReportSettingStuckConfig == defaultStuckReportSettings
          then Nothing
          else Just $ backToStuckReportSettingsuration smosReportSettingStuckConfig,
      confWorkReportConf =
        if smosReportSettingWorkConfig == defaultWorkReportSettings
          then Nothing
          else Just $ backToWorkReportSettingsuration smosReportSettingWorkConfig
    }

data WaitingReportSettingsuration = WaitingReportSettingsuration
  { waitingReportConfThreshold :: !(Maybe Time)
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via (Autodocodec WaitingReportSettingsuration)

instance Validity WaitingReportSettingsuration

instance HasCodec WaitingReportSettingsuration where
  codec =
    object "WaitingReportSettingsuration" $
      WaitingReportSettingsuration
        <$> optionalFieldOrNull "threshold" "waiting report threshold to consider waiting entries 'overdue'" .= waitingReportConfThreshold

backToWaitingReportSettingsuration :: WaitingReportSettings -> WaitingReportSettingsuration
backToWaitingReportSettingsuration WaitingReportSettings {..} =
  WaitingReportSettingsuration
    { waitingReportConfThreshold =
        if waitingReportSettingThreshold == defaultWaitingThreshold
          then Nothing
          else Just defaultWaitingThreshold
    }

data StuckReportSettingsuration = StuckReportSettingsuration
  { stuckReportConfThreshold :: !(Maybe Time)
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via (Autodocodec StuckReportSettingsuration)

instance Validity StuckReportSettingsuration

instance HasCodec StuckReportSettingsuration where
  codec =
    object "StuckReportSettingsuration" $
      StuckReportSettingsuration
        <$> optionalFieldOrNull "threshold" "stuck report threshold to consider stuck projects 'overdue'" .= stuckReportConfThreshold

backToStuckReportSettingsuration :: StuckReportSettings -> StuckReportSettingsuration
backToStuckReportSettingsuration StuckReportSettings {..} =
  StuckReportSettingsuration
    { stuckReportConfThreshold =
        if stuckReportSettingThreshold == defaultStuckThreshold
          then Nothing
          else Just defaultStuckThreshold
    }

data WorkReportSettingsuration = WorkReportSettingsuration
  { workReportConfBaseFilter :: !(Maybe EntryFilter),
    workReportConfChecks :: !(Maybe (Set EntryFilter)),
    workReportConfContexts :: !(Maybe (Map ContextName EntryFilter)),
    workReportConfTimeFilterProperty :: Maybe PropertyName,
    workReportConfProjection :: Maybe (NonEmpty Projection),
    workReportConfSorter :: Maybe Sorter
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec WorkReportSettingsuration)

instance Validity WorkReportSettingsuration

instance HasCodec WorkReportSettingsuration where
  codec =
    object "WorkReportSettingsuration" $
      WorkReportSettingsuration
        <$> optionalFieldOrNull "base-filter" "The base work filter" .= workReportConfBaseFilter
        <*> optionalFieldOrNull "checks" "Checks for the work report" .= workReportConfChecks
        <*> optionalFieldOrNull "contexts" "Contexts for the work report" .= workReportConfContexts
        <*> optionalFieldOrNull "time-filter" "The property to use to filter by time" .= workReportConfTimeFilterProperty
        <*> optionalFieldOrNull "columns" "The columns in the report" .= workReportConfProjection
        <*> optionalFieldOrNull "sorter" "The sorter to use to sort the rows" .= workReportConfSorter

defaultWorkReportSettingsuration :: WorkReportSettingsuration
defaultWorkReportSettingsuration =
  WorkReportSettingsuration
    { workReportConfBaseFilter = Nothing,
      workReportConfChecks = Nothing,
      workReportConfContexts = Nothing,
      workReportConfTimeFilterProperty = Nothing,
      workReportConfProjection = Nothing,
      workReportConfSorter = Nothing
    }

backToWorkReportSettingsuration :: WorkReportSettings -> WorkReportSettingsuration
backToWorkReportSettingsuration WorkReportSettings {..} =
  WorkReportSettingsuration
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
