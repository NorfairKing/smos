{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-pattern-binds #-}

module Smos.Scheduler.Schedule
  ( Schedule (..),
    ScheduleItem (..),
    DestinationPathTemplate (..),
    ScheduleTemplate (..),
    TimestampTemplate (..),
    UTCTimeTemplate (..),
    ScheduleItemName,
    EntryTemplate (..),
    readScheduleTemplate,
    writeScheduleTemplate,
    Recurrence (..),
  )
where

import Autodocodec
import Control.Arrow (left)
import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as SB
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.String
import Data.Text (Text)
import Data.Tree
import Data.Validity
import Data.Validity.Path ()
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import OptEnvConf
import Path
import Path.IO
import Smos.CLI.Colour
import Smos.CLI.OptParse
import Smos.Data
import Smos.Directory.OptParse
import Smos.Report.Time
import System.Cron (CronSchedule, parseCronSchedule, serializeCronSchedule)
import UnliftIO.IO.File

newtype Schedule = Schedule
  { scheduleItems :: Map ScheduleItemName ScheduleItem
  }

type ScheduleItemName = PropertyValue

data ScheduleItem = ScheduleItem
  { scheduleItemDescription :: !(Maybe Text),
    scheduleItemTemplate :: !FilePath,
    scheduleItemDestination :: !DestinationPathTemplate,
    scheduleItemRecurrence :: !Recurrence
  }
  deriving (Show, Generic)

instance Validity ScheduleItem

instance HasCodec ScheduleItem where
  codec =
    object "ScheduleItem" $
      ScheduleItem
        <$> optionalFieldOrNull "description" "A description of this item"
          .= scheduleItemDescription
        <*> requiredField "template" "The file to copy from (absolute or relative, inside the workflow directory)"
          .= scheduleItemTemplate
        <*> requiredField "destination" "The file to copy to (relative, inside the workflow directory)"
          .= scheduleItemDestination
        <*> requiredField "schedule" "The schedule on which to do the copying"
          .= scheduleItemRecurrence

instance Validity CronSchedule where
  validate = trivialValidation

newtype DestinationPathTemplate = DestinationPathTemplate {destinationPathTemplatePath :: Path Rel File}
  deriving (Show, Generic)

instance Validity DestinationPathTemplate

instance HasCodec DestinationPathTemplate where
  codec = dimapCodec DestinationPathTemplate destinationPathTemplatePath codec

data Instructions = Instructions Dispatch Settings

instance HasParser Instructions where
  settingsParser =
    withSmosConfig $
      Instructions
        <$> settingsParser
        <*> settingsParser

data Dispatch
  = DispatchCheck
  | DispatchSample !(Path Abs File) !(Maybe DestinationPathTemplate)
  | DispatchSchedule
  | DispatchNext

instance HasParser Dispatch where
  settingsParser =
    commands
      [ command "check" "Check that all schedules are valid" $
          pure DispatchCheck,
        command "sample" "Produce a sample scheduled project being filled in" $
          DispatchSample
            <$> filePathSetting
              [ help "template to fill in",
                argument
              ]
            <*> optional
              ( setting
                  [ help "destination path template. Note that the rendered template will be written here",
                    reader $ maybeReader $ fmap DestinationPathTemplate . parseRelFile,
                    option,
                    long "destination",
                    metavar "PATH_TEMPLATE"
                  ]
              ),
        command "schedule" "Run the schedules" $
          pure DispatchSchedule,
        command "next" "List the next times that scheduled will be activated" $
          pure DispatchNext
      ]

data Settings = Settings
  { setDirectorySettings :: !DirectorySettings,
    setSchedule :: !Schedule,
    setColourSettings :: !ColourSettings
  }

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: OptEnvConf.Parser Settings
parseSettings = do
  setDirectorySettings <- settingsParser
  setColourSettings <- settingsParser
  let sub = subConfig_ "scheduler" . subEnv_ "scheduler"
  setSchedule <-
    sub $
      Schedule
        <$> setting
          [ help "Schedule on which to schedule projects",
            conf "schedule",
            value M.empty
          ]
  pure Settings {..}

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
                <$> optionalFieldOrNullWithOmittedDefault' "header" emptyHeader
                  .= entryTemplateHeader
                <*> optionalFieldOrNull' "contents"
                  .= entryTemplateContents
                <*> optionalFieldOrNullWithOmittedDefault' "timestamps" M.empty
                  .= entryTemplateTimestamps
                <*> optionalFieldOrNullWithOmittedDefault' "properties" M.empty
                  .= entryTemplateProperties
                <*> optionalField' "state"
                  .= entryTemplateState
                <*> optionalFieldOrNullWithOmittedDefault' "tags" S.empty
                  .= entryTemplateTags
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
  deriving stock (Show, Eq, Generic)
  deriving newtype (IsString)
  deriving (FromJSON, ToJSON) via (Autodocodec TimestampTemplate)

instance Validity TimestampTemplate

instance HasCodec TimestampTemplate where
  codec = dimapCodec TimestampTemplate timestampTemplateText codec

newtype UTCTimeTemplate = UTCTimeTemplate
  { utcTimeTemplateText :: Text
  }
  deriving stock (Show, Eq, Generic)
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
  deriving stock (Show, Generic)

instance Validity Recurrence

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
