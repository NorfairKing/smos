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
import System.Cron (CronSchedule, parseCronSchedule, serializeCronSchedule)

data Arguments = Arguments Command (Report.FlagsWithConfigFile Flags)
  deriving (Show, Eq)

data Command
  = CommandCheck
  | CommandNext
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

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> Report.directoryConfigurationObjectCodec .= confDirectoryConfiguration
        <*> optionalField colourConfigurationKey "The colour configuration" .= confColourConfiguration
        <*> optionalField "scheduler" "The scheduler configuration" .= confSchedulerConfiguration

data SchedulerConfiguration = SchedulerConfiguration
  { schedulerConfStateFile :: !(Maybe FilePath),
    schedulerConfSchedule :: !(Maybe Schedule)
  }
  deriving (Show, Eq)

instance HasCodec SchedulerConfiguration where
  codec =
    object "SchedulerConfiguration" $
      SchedulerConfiguration
        <$> optionalField "state-file" "The file to store the scheduler state in" .= schedulerConfStateFile
        <*> optionalField "schedule" "The scheduler schedule" .= schedulerConfSchedule

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
        <$> optionalField "description" "A description of this item" .= scheduleItemDescription
        <*> requiredField "template" "The file to copy from (relative, inside the workflow directory)" .= scheduleItemTemplate
        <*> requiredField "destination" "The file to copy to (relative, inside the workflow directory)" .= scheduleItemDestination
        <*> requiredFieldWith "schedule" (bimapCodec parseCronSchedule serializeCronSchedule codec) "The schedule on which to do the copying" .= scheduleItemCronSchedule

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
        <*> optionalFieldWithOmittedDefault "item-last-runs" M.empty "when each schedule item was last run" .= scheduleStateLastRuns

newtype ScheduleItemHash = ScheduleItemHash {unScheduleItemHash :: Word64}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromJSONKey, ToJSONKey)

hashScheduleItem :: ScheduleItem -> ScheduleItemHash
hashScheduleItem = ScheduleItemHash . (fromIntegral :: Int -> Word64) . hash

newtype ScheduleTemplate = ScheduleTemplate
  { scheduleTemplateForest :: Forest EntryTemplate
  }
  deriving (Show, Eq, Generic)

instance Validity ScheduleTemplate

instance HasCodec ScheduleTemplate where
  codec = dimapCodec ScheduleTemplate scheduleTemplateForest $ entryForestCodec "EntryTemplate" codec

data EntryTemplate = EntryTemplate
  { entryTemplateHeader :: Header,
    entryTemplateContents :: Maybe Contents,
    entryTemplateTimestamps :: Map TimestampName TimestampTemplate,
    entryTemplateProperties :: Map PropertyName PropertyValue,
    entryTemplateState :: Maybe (Maybe TodoState),
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

instance HasCodec EntryTemplate where
  codec =
    dimapCodec f g $
      eitherCodec
        (codec <?> "A header-only entry template")
        ( object "EntryTemplate" $
            EntryTemplate
              <$> optionalFieldWithOmittedDefault' "header" emptyHeader .= entryTemplateHeader
              <*> optionalField' "contents" .= entryTemplateContents
              <*> optionalFieldWithOmittedDefault' "timestamps" M.empty .= entryTemplateTimestamps
              <*> optionalFieldWithOmittedDefault' "properties" M.empty .= entryTemplateProperties
              <*> optionalFieldWithOmittedDefault' "state" Nothing .= entryTemplateState
              <*> optionalFieldWithOmittedDefault' "tags" S.empty .= entryTemplateTags
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

instance Validity TimestampTemplate

instance HasCodec TimestampTemplate where
  codec = dimapCodec TimestampTemplate timestampTemplateText codec

newtype UTCTimeTemplate = UTCTimeTemplate
  { utcTimeTemplateText :: Text
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (IsString)

instance Validity UTCTimeTemplate

instance HasCodec UTCTimeTemplate where
  codec = dimapCodec UTCTimeTemplate utcTimeTemplateText codec
