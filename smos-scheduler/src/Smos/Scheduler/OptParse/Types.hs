{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Scheduler.OptParse.Types where

import GHC.Generics (Generic)

import Data.Aeson
import Path

import Smos.Report.Config as Report
import qualified Smos.Report.OptParse.Types as Report

data Flags =
  Flags
    { flagReportFlags :: Report.Flags
    , flagStateFile :: Maybe FilePath
    }
  deriving (Show, Eq)

data Configuration =
  Configuration
    { confReportConfiguration :: Report.Configuration
    , confSchedulerConfiguration :: Maybe SchedulerConfiguration
    }
  deriving (Show, Eq)

instance FromJSON Configuration where
  parseJSON v =
    flip (withObject "Configuration") v $ \o -> Configuration <$> parseJSON v <*> o .:? "scheduler"

data SchedulerConfiguration =
  SchedulerConfiguration
    { schedulerConfStateFile :: Maybe FilePath
    , schedulerConfSchedule :: Maybe Schedule
    }
  deriving (Show, Eq)

instance FromJSON SchedulerConfiguration where
  parseJSON =
    withObject "SchedulerConfiguration" $ \o ->
      SchedulerConfiguration <$> o .:? "state-file" <*> o .:? "schedule"

newtype Schedule =
  Schedule
    { scheduleItems :: [ScheduleItem]
    }
  deriving (Show, Eq, Generic, FromJSON)

data ScheduleItem =
  ScheduleItem
    { scheduleItemTemplate :: Path Rel File
    , scheduleItemDestination :: Path Rel File
    }
  deriving (Show, Eq, Generic)

instance FromJSON ScheduleItem where
  parseJSON =
    withObject "ScheduleItem" $ \o -> ScheduleItem <$> o .: "template" <*> o .: "destination"

data Environment =
  Environment
    { envReportEnvironment :: Report.Environment
    , envStateFile :: Maybe FilePath
    }
  deriving (Show, Eq)

data Settings =
  Settings
    { setReportSettings :: SmosReportConfig
    , setStateFile :: Path Abs File
    , setSchedule :: Schedule
    }
  deriving (Show, Eq)
