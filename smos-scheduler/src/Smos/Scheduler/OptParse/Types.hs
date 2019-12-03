{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Scheduler.OptParse.Types where

import GHC.Generics (Generic)

import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Time
import Data.Tree
import Path

import Control.Applicative

import System.Cron (CronSchedule, parseCronSchedule)

import Smos.Data

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
    , scheduleItemCronSchedule :: CronSchedule
    }
  deriving (Show, Eq, Generic)

instance FromJSON ScheduleItem where
  parseJSON =
    withObject "ScheduleItem" $ \o ->
      ScheduleItem <$> o .: "template" <*> o .: "destination" <*>
      (do t <- o .: "schedule"
          case parseCronSchedule t of
            Left err -> fail err
            Right cs -> pure cs)

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

data ScheduleState =
  ScheduleState
    { scheduleStateLastRun :: UTCTime
    }
  deriving (Show, Eq, Generic)

instance ToJSON ScheduleState where
  toJSON ScheduleState {..} = object ["last-run" .= scheduleStateLastRun]

instance FromJSON ScheduleState where
  parseJSON = withObject "ScheduleState" $ \o -> ScheduleState <$> o .: "last-run"

newtype ScheduleTemplate =
  ScheduleTemplate
    { scheduleTemplateForest :: Forest EntryTemplate
    }
  deriving (Show, Eq)

instance FromJSON ScheduleTemplate where
  parseJSON v = ScheduleTemplate . unForYaml <$> parseJSON v

instance FromJSON (ForYaml (Tree EntryTemplate)) where
  parseJSON v =
    fmap ForYaml $
    case v of
      Object o -> do
        mv <- o .:? "entry"
                -- This marks that we want to be trying to parse a tree and NOT an entry.
                -- We force the parser to make a decision this way.
        case mv :: Maybe Value of
          Nothing -> Node <$> parseJSON v <*> pure []
          Just _ ->
            (withObject "Tree Entry" $ \o' ->
               Node <$> o .: "entry" <*> (unForYaml <$> o' .:? "forest" .!= ForYaml []))
              v
      _ -> Node <$> parseJSON v <*> pure []

data EntryTemplate =
  EntryTemplate
    { entryTemplateHeader :: Header
    , entryTemplateContents :: Maybe Contents
    , entryTemplateTimestamps :: Map TimestampName TimestampTemplate
    , entryTemplateProperties :: Map PropertyName PropertyValue
    , entryTemplateStateHistory :: StateHistoryTemplate
    , entryTemplateTags :: Set Tag
    }
  deriving (Show, Eq, Generic)

newEntryTemplate :: Header -> EntryTemplate
newEntryTemplate h =
  EntryTemplate
    { entryTemplateHeader = h
    , entryTemplateContents = Nothing
    , entryTemplateTimestamps = M.empty
    , entryTemplateProperties = M.empty
    , entryTemplateStateHistory = emptyStateHistoryTemplate
    , entryTemplateTags = S.empty
    }

instance FromJSON EntryTemplate where
  parseJSON v =
    (do h <- parseJSON v
        pure $ newEntryTemplate h) <|>
    (withObject "EntryTemplate" $ \o ->
       EntryTemplate <$> o .:? "header" .!= emptyHeader <*> o .:? "contents" <*>
       o .:? "timestamps" .!= M.empty <*>
       o .:? "properties" .!= M.empty <*>
       o .:? "state-history" .!= emptyStateHistoryTemplate <*>
       o .:? "tags" .!= S.empty)
      v

newtype TimestampTemplate =
  TimestampTemplate
    { timestampTemplateText :: Text
    }
  deriving (Show, Eq, Ord, Generic, FromJSON)

newtype StateHistoryTemplate =
  StateHistoryTemplate
    { stateHistoryEntryTemplates :: [StateHistoryEntryTemplate]
    }
  deriving (Show, Eq, Generic, FromJSON)

emptyStateHistoryTemplate :: StateHistoryTemplate
emptyStateHistoryTemplate = StateHistoryTemplate {stateHistoryEntryTemplates = []}

data StateHistoryEntryTemplate =
  StateHistoryEntryTemplate
    { stateHistoryEntryTemplateNewState :: Maybe TodoState
    , stateHistoryEntryTemplateTimestamp :: UTCTimeTemplate
    }
  deriving (Show, Eq, Generic)

instance FromJSON StateHistoryEntryTemplate where
  parseJSON =
    withObject "StateHistoryEntryTemplate" $ \o ->
      StateHistoryEntryTemplate <$> o .: "new-state" <*> o .: "timestamp"

newtype UTCTimeTemplate =
  UTCTimeTemplate
    { utcTimeTemplateText :: Text
    }
  deriving (Show, Eq, Ord, Generic, FromJSON)
