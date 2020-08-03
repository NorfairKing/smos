{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.RecurringEvent where

import Data.Aeson
import Data.Text (Text)
import Data.Time
import Data.Validity
import GHC.Generics
import Smos.Data
import YamlParse.Applicative

data RecurringEvent
  = RecurringEvent
      { recurringEventSummary :: !(Maybe Text),
        recurringEventDescription :: !(Maybe Text),
        recurringEventStart :: !(Maybe CalTimestamp),
        recurringEventEnd :: !(Maybe CalEndDuration)
      }
  deriving (Show, Eq, Generic)

instance Validity RecurringEvent

instance YamlSchema RecurringEvent where
  yamlSchema =
    objectParser "RecurringEvent" $
      RecurringEvent
        <$> optionalField' "summary"
        <*> optionalField' "description"
        <*> optionalField' "start"
        <*> optionalField' "end"

instance FromJSON RecurringEvent where
  parseJSON = viaYamlSchema

instance ToJSON RecurringEvent where
  toJSON RecurringEvent {..} =
    object
      [ "summary" .= recurringEventSummary,
        "description" .= recurringEventDescription,
        "start" .= recurringEventStart,
        "end" .= recurringEventEnd
      ]

data CalEndDuration
  = CalTimestamp CalTimestamp
  | CalDuration Int -- Seconds
  deriving (Show, Eq, Generic)

instance Validity CalEndDuration

instance YamlSchema CalEndDuration where
  yamlSchema =
    alternatives
      [ CalTimestamp <$> yamlSchema,
        CalDuration <$> yamlSchema
      ]

instance FromJSON CalEndDuration where
  parseJSON = viaYamlSchema

instance ToJSON CalEndDuration where
  toJSON = \case
    CalTimestamp cts -> toJSON cts
    CalDuration ndt -> toJSON ndt

data CalTimestamp
  = CalDate Day
  | CalDateTime CalDateTime
  deriving (Show, Eq, Generic)

instance Validity CalTimestamp

instance YamlSchema CalTimestamp where
  yamlSchema =
    alternatives
      [ CalDate <$> daySchema,
        CalDateTime <$> yamlSchema
      ]

instance FromJSON CalTimestamp where
  parseJSON = viaYamlSchema

instance ToJSON CalTimestamp where
  toJSON = \case
    CalDate d -> toJSON d
    CalDateTime dt -> toJSON dt

-- https://tools.ietf.org/html/rfc5545#section-3.3.5
data CalDateTime
  = Floating LocalTime
  | UTC UTCTime
  | Zoned LocalTime UTCOffset
  deriving (Show, Eq, Generic)

instance Validity CalDateTime

instance YamlSchema CalDateTime where
  yamlSchema =
    alternatives
      [ objectParser "CalDateTime" $
          alternatives
            [ Floating <$> requiredFieldWith' "floating" localTimeSchema,
              UTC <$> requiredFieldWith' "utc" (localTimeToUTC utc <$> localTimeSchema),
              Zoned <$> requiredFieldWith' "local" localTimeSchema <*> requiredField' "offset"
            ],
        Floating <$> localTimeSchema
      ]

instance FromJSON CalDateTime where
  parseJSON = viaYamlSchema

instance ToJSON CalDateTime where
  toJSON = \case
    Floating lt -> object ["floating" .= formatTime defaultTimeLocale timestampLocalTimeFormat lt]
    UTC utct -> object ["utc" .= formatTime defaultTimeLocale timestampLocalTimeFormat utct]
    Zoned lt o ->
      object
        [ "local" .= formatTime defaultTimeLocale timestampLocalTimeFormat lt,
          "offset" .= o
        ]

newtype UTCOffset = UTCOffset Int -- Seconds from UTCTime
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance Validity UTCOffset

instance YamlSchema UTCOffset where
  yamlSchema = UTCOffset <$> yamlSchema
