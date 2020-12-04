{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Scheduler.OptParse.Types where

import Control.Applicative
import Data.Aeson hiding ((<?>))
import Data.Hashable
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.String
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

data Flags = Flags
  { flagDirectoryFlags :: !Report.DirectoryFlags,
    flagStateFile :: !(Maybe FilePath)
  }
  deriving (Show, Eq)

data Configuration = Configuration
  { confDirectoryConfiguration :: !Report.DirectoryConfiguration,
    confSchedulerConfiguration :: !(Maybe SchedulerConfiguration)
  }
  deriving (Show, Eq)

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

instance YamlSchema Configuration where
  yamlSchema = Configuration <$> yamlSchema <*> objectParser "Configuration" (optionalField "scheduler" "The scheduler configuration")

data SchedulerConfiguration = SchedulerConfiguration
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

newtype Schedule = Schedule
  { scheduleItems :: [ScheduleItem]
  }
  deriving (Show, Eq, Generic, FromJSON)

instance YamlSchema Schedule where
  yamlSchema = Schedule <$> yamlSchema

data ScheduleItem = ScheduleItem
  { scheduleItemDescription :: !(Maybe Text),
    scheduleItemTemplate :: !(Path Rel File),
    scheduleItemDestination :: !DestinationPathTemplate,
    scheduleItemCronSchedule :: !CronSchedule
  }
  deriving (Show, Eq, Generic)

instance Validity ScheduleItem

instance Hashable ScheduleItem where
  hashWithSalt s (ScheduleItem _ t d cs) =
    -- Don't hash the description, on purpose
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
        <$> optionalField "description" "A description of this item"
        <*> requiredField "template" "The file to copy from (relative, inside the workflow directory)"
        <*> requiredField "destination" "The file to copy to (relative, inside the workflow directory)"
        <*> requiredFieldWith "schedule" "The schedule on which to do the copying" (eitherParser parseCronSchedule yamlSchema)

instance Validity CronSchedule where
  validate = trivialValidation

newtype DestinationPathTemplate = DestinationPathTemplate {destinationPathTemplatePath :: Path Rel File}
  deriving (Show, Eq, Generic)

instance Validity DestinationPathTemplate

instance Hashable DestinationPathTemplate

instance YamlSchema DestinationPathTemplate where
  yamlSchema = DestinationPathTemplate <$> yamlSchema

data Environment = Environment
  { envDirectoryEnvironment :: !Report.DirectoryEnvironment,
    envStateFile :: !(Maybe FilePath)
  }
  deriving (Show, Eq)

data Instructions = Instructions Dispatch Settings
  deriving (Show, Eq)

data Dispatch = DispatchCheck | DispatchSchedule
  deriving (Show, Eq)

data Settings = Settings
  { setDirectorySettings :: !Report.DirectoryConfig,
    setStateFile :: !(Path Abs File),
    setSchedule :: !Schedule
  }
  deriving (Show, Eq)

data ScheduleState = ScheduleState
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

newtype ScheduleTemplate = ScheduleTemplate
  { scheduleTemplateForest :: Forest EntryTemplate
  }
  deriving (Show, Eq, Generic)

instance Validity ScheduleTemplate

instance ToJSON ScheduleTemplate where
  toJSON = toJSON . ForYaml . scheduleTemplateForest

instance FromJSON ScheduleTemplate where
  parseJSON v = ScheduleTemplate . unForYaml <$> parseJSON v

instance ToJSON (ForYaml (Tree EntryTemplate)) where
  toJSON (ForYaml Node {..}) =
    if null subForest
      then toJSON rootLabel
      else object ["entry" .= rootLabel, "forest" .= ForYaml subForest]

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

data EntryTemplate = EntryTemplate
  { entryTemplateHeader :: Header,
    entryTemplateContents :: Maybe Contents,
    entryTemplateTimestamps :: Map TimestampName TimestampTemplate,
    entryTemplateProperties :: Map PropertyName PropertyValue,
    entryTemplateState :: Maybe TodoState,
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
      entryTemplateState = Nothing,
      entryTemplateTags = S.empty
    }

instance ToJSON EntryTemplate where
  toJSON EntryTemplate {..} =
    object
      [ "header" .= entryTemplateHeader,
        "contents" .= entryTemplateContents,
        "timestamps" .= entryTemplateTimestamps,
        "properties" .= entryTemplateProperties,
        "state" .= entryTemplateState,
        "tags" .= entryTemplateTags
      ]

instance FromJSON EntryTemplate where
  parseJSON v =
    ( do
        h <- parseJSON v
        pure $ newEntryTemplate h
    )
      <|> ( withObject "EntryTemplate" $ \o ->
              EntryTemplate
                <$> o .:? "header" .!= emptyHeader
                <*> o .:? "contents"
                <*> o .:? "timestamps" .!= M.empty
                <*> o .:? "properties" .!= M.empty
                <*> o .:? "state" .!= Nothing
                <*> o .:? "tags" .!= S.empty
          )
        v

instance YamlSchema EntryTemplate where
  yamlSchema =
    alternatives
      [ newEntryTemplate <$> (yamlSchema <?> "A header-only entry template"),
        objectParser "EntryTemplate" $
          EntryTemplate
            <$> optionalFieldWithDefault' "header" emptyHeader
            <*> optionalField' "contents"
            <*> optionalFieldWithDefault' "timestamps" M.empty
            <*> optionalFieldWithDefault' "properties" M.empty
            <*> optionalFieldWithDefault' "state" Nothing
            <*> optionalFieldWithDefault' "tags" S.empty
      ]

newtype TimestampTemplate = TimestampTemplate
  { timestampTemplateText :: Text
  }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON, IsString)

instance Validity TimestampTemplate

instance YamlSchema TimestampTemplate where
  yamlSchema = TimestampTemplate <$> yamlSchema

newtype UTCTimeTemplate = UTCTimeTemplate
  { utcTimeTemplateText :: Text
  }
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON, IsString)

instance Validity UTCTimeTemplate

instance YamlSchema UTCTimeTemplate where
  yamlSchema = UTCTimeTemplate <$> yamlSchema
