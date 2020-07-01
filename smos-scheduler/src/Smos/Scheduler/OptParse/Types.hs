{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Scheduler.OptParse.Types where

import Control.Applicative
import Data.Aeson
import Data.Hashable
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Time
import Data.Tree
import Data.Validity
import Data.Word
import GHC.Generics (Generic)
import Path
import Smos.Data
import Smos.Report.Config as Report
import qualified Smos.Report.OptParse.Types as Report
import System.Cron (CronSchedule, parseCronSchedule, serializeCronSchedule)
import YamlParse.Applicative

data Arguments = Arguments Command (Report.FlagsWithConfigFile Flags)
  deriving (Show, Eq)

data Command = CommandCheck | CommandSchedule
  deriving (Show, Eq)

data Flags
  = Flags
      { flagDirectoryFlags :: !Report.DirectoryFlags,
        flagStateFile :: !(Maybe FilePath)
      }
  deriving (Show, Eq)

data Configuration
  = Configuration
      { confDirectoryConfiguration :: !Report.DirectoryConfiguration,
        confSchedulerConfiguration :: !(Maybe SchedulerConfiguration)
      }
  deriving (Show, Eq)

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

instance YamlSchema Configuration where
  yamlSchema = Configuration <$> yamlSchema <*> objectParser "Configuration" (optionalField "scheduler" "The scheduler configuration")

data SchedulerConfiguration
  = SchedulerConfiguration
      { schedulerConfStateFile :: !(Maybe FilePath),
        schedulerConfSchedule :: !(Maybe Schedule)
      }
  deriving (Show, Eq)

instance FromJSON SchedulerConfiguration where
  parseJSON = viaYamlSchema

instance YamlSchema SchedulerConfiguration where
  yamlSchema =
    objectParser "SchedulerConfiguration" $
      SchedulerConfiguration
        <$> optionalField "state-file" "The file to store the scheduler state in"
        <*> optionalField "schedule" "The scheduler schedule"

newtype Schedule
  = Schedule
      { scheduleItems :: [ScheduleItem]
      }
  deriving (Show, Eq, Generic, FromJSON)

instance YamlSchema Schedule where
  yamlSchema = Schedule <$> yamlSchema

data ScheduleItem
  = ScheduleItem
      { scheduleItemTemplate :: !(Path Rel File),
        scheduleItemDestination :: !(Path Rel File), -- Turn this into a newline
        scheduleItemCronSchedule :: !CronSchedule
      }
  deriving (Show, Eq, Generic)

instance Validity ScheduleItem

instance Hashable ScheduleItem where
  hashWithSalt s (ScheduleItem t d cs) =
    s
      `hashWithSalt` t
      `hashWithSalt` d
      `hashWithSalt` serializeCronSchedule cs

instance FromJSON ScheduleItem where
  parseJSON = viaYamlSchema

instance YamlSchema ScheduleItem where
  yamlSchema =
    objectParser "ScheduleItem" $
      ScheduleItem
        <$> requiredField "template" "The file to copy from (relative, inside the workflow directory)"
        <*> requiredField "destination" "The file to copy to (relative, inside the workflow directory)"
        <*> requiredFieldWith "schedule" "The schedule on which to do the copying" (eitherParser parseCronSchedule yamlSchema)

instance Validity CronSchedule where
  validate = trivialValidation

data Environment
  = Environment
      { envDirectoryEnvironment :: !Report.DirectoryEnvironment,
        envStateFile :: !(Maybe FilePath)
      }
  deriving (Show, Eq)

data Instructions = Instructions Dispatch Settings
  deriving (Show, Eq)

data Dispatch = DispatchCheck | DispatchSchedule
  deriving (Show, Eq)

data Settings
  = Settings
      { setDirectorySettings :: !Report.DirectoryConfig,
        setStateFile :: !(Path Abs File),
        setSchedule :: !Schedule
      }
  deriving (Show, Eq)

data ScheduleState
  = ScheduleState
      { scheduleStateLastRun :: UTCTime,
        scheduleStateLastRuns :: Map ScheduleItemHash UTCTime
      }
  deriving (Show, Eq, Generic)

instance ToJSON ScheduleState where
  toJSON ScheduleState {..} = object ["last-run" .= scheduleStateLastRun, "item-last-runs" .= scheduleStateLastRuns]

instance FromJSON ScheduleState where
  parseJSON = withObject "ScheduleState" $ \o -> ScheduleState <$> o .: "last-run" <*> o .: "item-last-runs"

newtype ScheduleItemHash = ScheduleItemHash Word64
  deriving (Show, Eq, Ord, Generic, FromJSONKey, ToJSONKey)

hashScheduleItem :: ScheduleItem -> ScheduleItemHash
hashScheduleItem = ScheduleItemHash . (fromIntegral :: Int -> Word64) . hash

newtype ScheduleTemplate
  = ScheduleTemplate
      { scheduleTemplateForest :: Forest EntryTemplate
      }
  deriving (Show, Eq, Generic)

instance Validity ScheduleTemplate

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
              ( withObject "Tree Entry" $ \o' ->
                  Node <$> o .: "entry" <*> (unForYaml <$> o' .:? "forest" .!= ForYaml [])
              )
                v
        _ -> Node <$> parseJSON v <*> pure []

data EntryTemplate
  = EntryTemplate
      { entryTemplateHeader :: Header,
        entryTemplateContents :: Maybe Contents,
        entryTemplateTimestamps :: Map TimestampName TimestampTemplate,
        entryTemplateProperties :: Map PropertyName PropertyValue,
        entryTemplateStateHistory :: StateHistoryTemplate,
        entryTemplateTags :: Set Tag
      }
  deriving (Show, Eq, Generic)

instance Validity EntryTemplate

newEntryTemplate :: Header -> EntryTemplate
newEntryTemplate h =
  EntryTemplate
    { entryTemplateHeader = h,
      entryTemplateContents = Nothing,
      entryTemplateTimestamps = M.empty,
      entryTemplateProperties = M.empty,
      entryTemplateStateHistory = emptyStateHistoryTemplate,
      entryTemplateTags = S.empty
    }

instance FromJSON EntryTemplate where
  parseJSON v =
    ( do
        h <- parseJSON v
        pure $ newEntryTemplate h
    )
      <|> ( withObject "EntryTemplate" $ \o ->
              EntryTemplate <$> o .:? "header" .!= emptyHeader <*> o .:? "contents"
                <*> o .:? "timestamps" .!= M.empty
                <*> o .:? "properties" .!= M.empty
                <*> o .:? "state-history" .!= emptyStateHistoryTemplate
                <*> o .:? "tags" .!= S.empty
          )
        v

newtype TimestampTemplate
  = TimestampTemplate
      { timestampTemplateText :: Text
      }
  deriving (Show, Eq, Ord, Generic, FromJSON)

instance Validity TimestampTemplate

newtype StateHistoryTemplate
  = StateHistoryTemplate
      { stateHistoryEntryTemplates :: [StateHistoryEntryTemplate]
      }
  deriving (Show, Eq, Generic, FromJSON)

instance Validity StateHistoryTemplate

emptyStateHistoryTemplate :: StateHistoryTemplate
emptyStateHistoryTemplate = StateHistoryTemplate {stateHistoryEntryTemplates = []}

data StateHistoryEntryTemplate
  = StateHistoryEntryTemplate
      { stateHistoryEntryTemplateNewState :: Maybe TodoState,
        stateHistoryEntryTemplateTimestamp :: UTCTimeTemplate
      }
  deriving (Show, Eq, Generic)

instance Validity StateHistoryEntryTemplate

instance FromJSON StateHistoryEntryTemplate where
  parseJSON =
    withObject "StateHistoryEntryTemplate" $ \o ->
      StateHistoryEntryTemplate <$> o .: "new-state" <*> o .: "timestamp"

newtype UTCTimeTemplate
  = UTCTimeTemplate
      { utcTimeTemplateText :: Text
      }
  deriving (Show, Eq, Ord, Generic, FromJSON)

instance Validity UTCTimeTemplate
