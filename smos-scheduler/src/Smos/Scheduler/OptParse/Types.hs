{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Scheduler.OptParse.Types where

import Autodocodec
import Control.Arrow (left)
import Control.Monad
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import qualified Data.ByteString as SB
import Data.Hashable
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree
import Data.Validity
import Data.Word
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import Path
import Path.IO
import Smos.CLI.Colour
import Smos.CLI.OptParse
import Smos.Data
import Smos.Report.Config as Report
import qualified Smos.Report.OptParse.Types as Report
import Smos.Report.Time
import System.Cron (CronSchedule, parseCronSchedule, serializeCronSchedule)
import Text.Read
import UnliftIO.IO.File

data Arguments = Arguments Command (FlagsWithConfigFile Flags)
  deriving (Show, Eq)

data Command
  = CommandCheck
  | CommandNext
  | CommandSample !FilePath !(Maybe FilePath)
  | CommandSchedule
  deriving (Show, Eq)

data Flags = Flags
  { flagDirectoryFlags :: !Report.DirectoryFlags
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
        <$> objectCodec .= confDirectoryConfiguration
        <*> colourConfigurationTopLevelObjectCodec .= confColourConfiguration
        <*> optionalFieldOrNull "scheduler" "The scheduler configuration" .= confSchedulerConfiguration

data SchedulerConfiguration = SchedulerConfiguration
  { schedulerConfSchedule :: !(Maybe Schedule)
  }
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec SchedulerConfiguration)

instance HasCodec SchedulerConfiguration where
  codec =
    object "SchedulerConfiguration" $
      SchedulerConfiguration
        <$> optionalFieldOrNull "schedule" "The scheduler schedule" .= schedulerConfSchedule

newtype Schedule = Schedule
  { scheduleItems :: [ScheduleItem]
  }
  deriving (Show, Eq, Generic)

instance HasCodec Schedule where
  codec = dimapCodec Schedule scheduleItems codec

data ScheduleItem = ScheduleItem
  { scheduleItemDescription :: !(Maybe Text),
    scheduleItemTemplate :: !FilePath,
    scheduleItemDestination :: !DestinationPathTemplate,
    scheduleItemRecurrence :: !Recurrence
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec ScheduleItem)

instance Validity ScheduleItem

instance Hashable ScheduleItem where
  hashWithSalt s (ScheduleItem _ t d r) =
    -- Don't hash the description, on purpose
    s
      `hashWithSalt` t
      `hashWithSalt` d
      `hashWithSalt` r

instance HasCodec ScheduleItem where
  codec =
    object "ScheduleItem" $
      ScheduleItem
        <$> optionalFieldOrNull "description" "A description of this item" .= scheduleItemDescription
        <*> requiredField "template" "The file to copy from (absolute or relative, inside the workflow directory)" .= scheduleItemTemplate
        <*> requiredField "destination" "The file to copy to (relative, inside the workflow directory)" .= scheduleItemDestination
        <*> requiredField "schedule" "The schedule on which to do the copying" .= scheduleItemRecurrence

instance Validity CronSchedule where
  validate = trivialValidation

newtype DestinationPathTemplate = DestinationPathTemplate {destinationPathTemplatePath :: Path Rel File}
  deriving (Show, Eq, Generic)

instance Validity DestinationPathTemplate

instance Hashable DestinationPathTemplate

instance HasCodec DestinationPathTemplate where
  codec = dimapCodec DestinationPathTemplate destinationPathTemplatePath codec

data Environment = Environment
  { envDirectoryEnvironment :: !Report.DirectoryEnvironment
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
    setSchedule :: !Schedule,
    setColourSettings :: !ColourSettings
  }
  deriving (Show, Eq)

newtype ScheduleItemHash = ScheduleItemHash {unScheduleItemHash :: Word64}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromJSONKey, ToJSONKey)

instance Validity ScheduleItemHash

hashScheduleItem :: ScheduleItem -> ScheduleItemHash
hashScheduleItem = ScheduleItemHash . (fromIntegral :: Int -> Word64) . hash

renderScheduleItemHash :: ScheduleItemHash -> Text
renderScheduleItemHash = T.pack . show . unScheduleItemHash

parseScheduleItemHash :: Text -> Maybe ScheduleItemHash
parseScheduleItemHash = fmap ScheduleItemHash . readMaybe . T.unpack

writeScheduleTemplate :: Path Abs File -> ScheduleTemplate -> IO ()
writeScheduleTemplate p a = do
  ensureDir $ parent p
  writeBinaryFileDurableAtomic (fromAbsFile p) (Yaml.encode a)

readScheduleTemplate :: Path Abs File -> IO (Maybe (Either String ScheduleTemplate))
readScheduleTemplate f = do
  mContents <- forgivingAbsence $ SB.readFile $ fromAbsFile f
  forM mContents $ \cts ->
    pure $ left Yaml.prettyPrintParseException $ Yaml.decodeEither' cts

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

instance Hashable Recurrence where
  hashWithSalt s = \case
    RentRecurrence cs -> s `hashWithSalt` serializeCronSchedule cs
    HaircutRecurrence t -> s `hashWithSalt` (1 :: Int) `hashWithSalt` t

instance HasCodec Recurrence where
  codec =
    dimapCodec f g $
      eitherCodec
        (codec <?> "Haircut recurrence")
        (codec <?> "Rent recurrence")
    where
      f = \case
        Left ndc -> HaircutRecurrence ndc
        Right cs -> RentRecurrence cs
      g = \case
        HaircutRecurrence ndc -> Left ndc
        RentRecurrence cs -> Right cs

instance HasCodec CronSchedule where
  codec =
    bimapCodec parseCronSchedule serializeCronSchedule codec
      <?> "Cron schedule, see https://en.wikipedia.org/wiki/Cron#Overview"
