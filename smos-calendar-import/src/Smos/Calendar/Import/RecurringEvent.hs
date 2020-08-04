{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.RecurringEvent where

import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Data.Time
import Data.Validity
import GHC.Generics
import Smos.Data
import YamlParse.Applicative

data RecurringEvents
  = RecurringEvents
      { recurringEvents :: [RecurringEvent],
        recurringEventsTimeZones :: Map TimeZoneId TimeZoneHistory
      }
  deriving (Show, Eq, Generic)

instance Validity RecurringEvents

-- TODO validity constraints on timezone ids

instance YamlSchema RecurringEvents where
  yamlSchema =
    alternatives
      [ objectParser "RecurringEvents" $
          RecurringEvents
            <$> requiredField' "events"
            <*> optionalFieldWithDefault' "zones" M.empty,
        RecurringEvents <$> yamlSchema <*> pure M.empty
      ]

instance FromJSON RecurringEvents where
  parseJSON = viaYamlSchema

instance ToJSON RecurringEvents where
  toJSON RecurringEvents {..} =
    if M.null recurringEventsTimeZones
      then toJSON recurringEvents
      else object ["events" .= recurringEvents, "zones" .= recurringEventsTimeZones]

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
  | Zoned LocalTime TimeZoneId
  deriving (Show, Eq, Generic)

instance Validity CalDateTime

instance YamlSchema CalDateTime where
  yamlSchema =
    alternatives
      [ objectParser "CalDateTime" $
          alternatives
            [ Floating <$> requiredFieldWith' "floating" localTimeSchema,
              UTC <$> requiredFieldWith' "utc" (localTimeToUTC utc <$> localTimeSchema),
              Zoned <$> requiredFieldWith' "local" localTimeSchema <*> requiredField' "zone"
            ],
        Floating <$> localTimeSchema
      ]

instance FromJSON CalDateTime where
  parseJSON = viaYamlSchema

instance ToJSON CalDateTime where
  toJSON = \case
    Floating lt -> object ["floating" .= formatTime defaultTimeLocale timestampLocalTimeFormat lt]
    UTC utct -> object ["utc" .= formatTime defaultTimeLocale timestampLocalTimeFormat utct]
    Zoned lt tzid ->
      object
        [ "local" .= formatTime defaultTimeLocale timestampLocalTimeFormat lt,
          "zone" .= tzid
        ]

newtype TimeZoneId = TimeZoneId Text -- Name of the timezone
  deriving (Show, Eq, Ord, Generic, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

instance Validity TimeZoneId

instance YamlKeySchema TimeZoneId where
  yamlKeySchema = TimeZoneId <$> yamlKeySchema

instance YamlSchema TimeZoneId where
  yamlSchema = TimeZoneId <$> yamlSchema

data TimeZoneHistory
  = TimeZoneHistory
      { timeZoneHistoryStart :: LocalTime, -- In the timezone at the time
        timeZoneHistoryOffsetFrom :: UTCOffset,
        timeZoneHistoryOffsetTo :: UTCOffset
      }
  deriving (Show, Eq, Generic)

instance Validity TimeZoneHistory

instance YamlSchema TimeZoneHistory where
  yamlSchema =
    objectParser "TimeZoneHistory" $
      TimeZoneHistory
        <$> requiredFieldWith' "start" localTimeSchema
        <*> requiredField' "from"
        <*> requiredField' "to"

instance FromJSON TimeZoneHistory where
  parseJSON = viaYamlSchema

instance ToJSON TimeZoneHistory where
  toJSON TimeZoneHistory {..} =
    object
      [ "start" .= formatTime defaultTimeLocale timestampLocalTimeFormat timeZoneHistoryStart,
        "from" .= timeZoneHistoryOffsetFrom,
        "to" .= timeZoneHistoryOffsetTo
      ]

newtype UTCOffset = UTCOffset Int -- Seconds from UTCTime
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance Validity UTCOffset

instance YamlSchema UTCOffset where
  yamlSchema = UTCOffset <$> yamlSchema
