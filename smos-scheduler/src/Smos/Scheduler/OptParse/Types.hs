{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Scheduler.OptParse.Types where

import Autodocodec
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
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
import Smos.Query.OptParse.Types (ColourConfiguration (..), ColourSettings, colourConfigurationKey)
import Smos.Report.Config as Report
import qualified Smos.Report.OptParse.Types as Report
import Smos.Report.Time
import System.Cron (CronSchedule, parseCronSchedule, serializeCronSchedule)

data Arguments = Arguments Command (Report.FlagsWithConfigFile Flags)
  deriving (Show, Eq)

data Command
  = CommandCheck
  | CommandNext
  | CommandSample !FilePath !(Maybe FilePath)
  | CommandSchedule
  deriving (Show, Eq)

data Flags = Flags
  { flagDirectoryFlags :: !Report.DirectoryFlags,
    flagStateFile :: !(Maybe FilePath)
  }
  deriving (Show, Eq)

data Configuration = Configuration
  { confDirectoryConfiguration :: !Report.DirectoryConfiguration,
    confColourConfiguration :: !(Maybe ColourConfiguration),
    confSchedulerConfiguration :: !(Maybe SchedulerConfiguration)
  }
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec Configuration)

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> Report.directoryConfigurationObjectCodec .= confDirectoryConfiguration
        <*> optionalFieldOrNull colourConfigurationKey "The colour configuration" .= confColourConfiguration
        <*> optionalFieldOrNull "scheduler" "The scheduler configuration" .= confSchedulerConfiguration

data SchedulerConfiguration = SchedulerConfiguration
  { schedulerConfStateFile :: !(Maybe FilePath),
    schedulerConfSchedule :: !(Maybe Schedule)
  }
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec SchedulerConfiguration)

instance HasCodec SchedulerConfiguration where
  codec =
    object "SchedulerConfiguration" $
      SchedulerConfiguration
        <$> optionalFieldOrNull "state-file" "The file to store the scheduler state in" .= schedulerConfStateFile
        <*> optionalFieldOrNull "schedule" "The scheduler schedule" .= schedulerConfSchedule

newtype Schedule = Schedule
  { scheduleItems :: [ScheduleItem]
  }
  deriving (Show, Eq, Generic)

instance HasCodec Schedule where
  codec = dimapCodec Schedule scheduleItems codec

data ScheduleItem = ScheduleItem
  { scheduleItemDescription :: !(Maybe Text),
    scheduleItemTemplate :: !(Path Rel File),
    scheduleItemDestination :: !DestinationPathTemplate,
    scheduleItemCronSchedule :: !CronSchedule
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec ScheduleItem)

instance Validity ScheduleItem

instance Hashable ScheduleItem where
  hashWithSalt s (ScheduleItem _ t d cs) =
    -- Don't hash the description, on purpose
    s
      `hashWithSalt` t
      `hashWithSalt` d
      `hashWithSalt` serializeCronSchedule cs

instance HasCodec ScheduleItem where
  codec =
    object "ScheduleItem" $
      ScheduleItem
        <$> optionalFieldOrNull "description" "A description of this item" .= scheduleItemDescription
        <*> requiredField "template" "The file to copy from (relative, inside the workflow directory)" .= scheduleItemTemplate
        <*> requiredField "destination" "The file to copy to (relative, inside the workflow directory)" .= scheduleItemDestination
        <*> requiredField "schedule" "The schedule on which to do the copying" .= scheduleItemCronSchedule

instance Validity CronSchedule where
  validate = trivialValidation

newtype DestinationPathTemplate = DestinationPathTemplate {destinationPathTemplatePath :: Path Rel File}
  deriving (Show, Eq, Generic)

instance Validity DestinationPathTemplate

instance Hashable DestinationPathTemplate

instance HasCodec DestinationPathTemplate where
  codec = dimapCodec DestinationPathTemplate destinationPathTemplatePath codec

data Environment = Environment
  { envDirectoryEnvironment :: !Report.DirectoryEnvironment,
    envStateFile :: !(Maybe FilePath)
  }
  deriving (Show, Eq)

data Instructions = Instructions Dispatch Settings
  deriving (Show, Eq)

data Dispatch
  = DispatchCheck
  | DispatchNext
  | DispatchSample !(Path Abs File) !(Maybe DestinationPathTemplate)
  | DispatchSchedule
  deriving (Show, Eq)

data Settings = Settings
  { setDirectorySettings :: !Report.DirectoryConfig,
    setStateFile :: !(Path Abs File),
    setSchedule :: !Schedule,
    setColourSettings :: !ColourSettings
  }
  deriving (Show, Eq)

data ScheduleState = ScheduleState
  { scheduleStateLastRun :: UTCTime,
    scheduleStateLastRuns :: Map ScheduleItemHash UTCTime
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec ScheduleState)

instance HasCodec ScheduleState where
  codec =
    object "ScheduleState" $
      ScheduleState
        <$> requiredField "last-run" "when smos-scheduler was last run" .= scheduleStateLastRun
        <*> optionalFieldOrNullWithOmittedDefault "item-last-runs" M.empty "when each schedule item was last run" .= scheduleStateLastRuns

newtype ScheduleItemHash = ScheduleItemHash {unScheduleItemHash :: Word64}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromJSONKey, ToJSONKey)

hashScheduleItem :: ScheduleItem -> ScheduleItemHash
hashScheduleItem = ScheduleItemHash . (fromIntegral :: Int -> Word64) . hash

newtype ScheduleTemplate = ScheduleTemplate
  { scheduleTemplateForest :: Forest EntryTemplate
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec ScheduleTemplate)

instance Validity ScheduleTemplate

instance HasCodec ScheduleTemplate where
  codec =
    dimapCodec ScheduleTemplate scheduleTemplateForest $
      entryForestCodec "EntryTemplate" codec

data EntryTemplate = EntryTemplate
  { entryTemplateHeader :: Header,
    entryTemplateContents :: Maybe Contents,
    entryTemplateTimestamps :: Map TimestampName TimestampTemplate,
    entryTemplateProperties :: Map PropertyName PropertyValue,
    entryTemplateState :: Maybe (Maybe TodoState),
    entryTemplateTags :: Set Tag
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec EntryTemplate)

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

instance HasCodec EntryTemplate where
  codec =
    named "EntryTemplate" $
      dimapCodec f g $
        eitherCodec
          (codec <?> "A header-only entry template")
          ( object "EntryTemplate" $
              EntryTemplate
                <$> optionalFieldOrNullWithOmittedDefault' "header" emptyHeader .= entryTemplateHeader
                <*> optionalFieldOrNull' "contents" .= entryTemplateContents
                <*> optionalFieldOrNullWithOmittedDefault' "timestamps" M.empty .= entryTemplateTimestamps
                <*> optionalFieldOrNullWithOmittedDefault' "properties" M.empty .= entryTemplateProperties
                <*> optionalField' "state" .= entryTemplateState
                <*> optionalFieldOrNullWithOmittedDefault' "tags" S.empty .= entryTemplateTags
          )
    where
      f = \case
        Left h -> newEntryTemplate h
        Right et -> et
      g et =
        if et == newEntryTemplate (entryTemplateHeader et)
          then Left (entryTemplateHeader et)
          else Right et

newtype TimestampTemplate = TimestampTemplate
  { timestampTemplateText :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (IsString)
  deriving (FromJSON, ToJSON) via (Autodocodec TimestampTemplate)

instance Validity TimestampTemplate

instance HasCodec TimestampTemplate where
  codec = dimapCodec TimestampTemplate timestampTemplateText codec

newtype UTCTimeTemplate = UTCTimeTemplate
  { utcTimeTemplateText :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (IsString)
  deriving (FromJSON, ToJSON) via (Autodocodec UTCTimeTemplate)

instance Validity UTCTimeTemplate

instance HasCodec UTCTimeTemplate where
  codec = dimapCodec UTCTimeTemplate utcTimeTemplateText codec

data Recurrence
  = -- | This much time after the last one
    HaircutRecurrence !Time
  | -- | At these times
    RentRecurrence !CronSchedule
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Recurrence)

instance Validity Recurrence

instance HasCodec Recurrence where
  codec = dimapCodec f g $ eitherCodec codec codec
    where
      f = \case
        Left ndc -> HaircutRecurrence ndc
        Right cs -> RentRecurrence cs
      g = \case
        HaircutRecurrence ndc -> Left ndc
        RentRecurrence cs -> Right cs

instance HasCodec CronSchedule where
  codec = bimapCodec parseCronSchedule serializeCronSchedule codec
