{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.RecurringEvent where

import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as M
import Data.Time
import Data.Validity
import GHC.Generics
import Smos.Calendar.Import.Static
import Smos.Calendar.Import.TimeZone
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
      { recurringEventStatic :: !Static,
        recurringEventStart :: !(Maybe CalTimestamp),
        recurringEventEnd :: !(Maybe CalEndDuration)
      }
  deriving (Show, Eq, Generic)

instance Validity RecurringEvent

instance YamlSchema RecurringEvent where
  yamlSchema =
    objectParser "RecurringEvent" $
      RecurringEvent
        <$> staticObjectParser
        <*> optionalField' "start"
        <*> optionalField' "end"

instance FromJSON RecurringEvent where
  parseJSON = viaYamlSchema

instance ToJSON RecurringEvent where
  toJSON RecurringEvent {..} =
    object $
      staticToObject recurringEventStatic
        ++ [ "start" .= recurringEventStart,
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
