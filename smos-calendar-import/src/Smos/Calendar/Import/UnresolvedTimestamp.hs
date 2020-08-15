{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Calendar.Import.UnresolvedTimestamp where

import Data.Aeson
import Data.String
import Data.Text (Text)
import Data.Time
import Data.Validity
import GHC.Generics
import Smos.Data
import YamlParse.Applicative

data CalRDate
  = CalRTimestamp CalTimestamp
  | CalRPeriod CalPeriod
  deriving (Show, Eq, Ord, Generic)

instance Validity CalRDate

instance YamlSchema CalRDate where
  yamlSchema =
    alternatives
      [ CalRTimestamp <$> yamlSchema,
        CalRPeriod <$> yamlSchema
      ]

instance ToJSON CalRDate where
  toJSON = \case
    CalRTimestamp cts -> toJSON cts
    CalRPeriod cp -> toJSON cp

instance FromJSON CalRDate where
  parseJSON = viaYamlSchema

data CalPeriod
  = CalPeriodFromTo CalDateTime CalDateTime
  | CalPeriodDuration CalDateTime Int -- Seconds
  deriving (Show, Eq, Ord, Generic)

instance Validity CalPeriod

instance YamlSchema CalPeriod where
  yamlSchema =
    alternatives
      [ objectParser "period" $ CalPeriodFromTo <$> requiredField' "from" <*> requiredField' "to",
        objectParser "duration" $ CalPeriodDuration <$> requiredField' "from" <*> requiredField' "duration"
      ]

instance ToJSON CalPeriod where
  toJSON = \case
    CalPeriodFromTo from to -> object ["from" .= from, "to" .= to]
    CalPeriodDuration from dur -> object ["from" .= from, "duration" .= dur]

instance FromJSON CalPeriod where
  parseJSON = viaYamlSchema

data CalEndDuration
  = CalTimestamp CalTimestamp
  | CalDuration Int -- Seconds
  deriving (Show, Eq, Ord, Generic)

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
  deriving (Show, Eq, Ord, Generic)

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
  deriving (Show, Eq, Ord, Generic)

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
    Floating lt -> toJSON $ formatTime defaultTimeLocale timestampLocalTimeFormat lt
    UTC utct -> object ["utc" .= formatTime defaultTimeLocale timestampLocalTimeFormat utct]
    Zoned lt tzid ->
      object
        [ "local" .= formatTime defaultTimeLocale timestampLocalTimeFormat lt,
          "zone" .= tzid
        ]

newtype TimeZoneId = TimeZoneId Text -- Unique id of the timezone
  deriving (Show, Eq, Ord, Generic, FromJSON, FromJSONKey, ToJSON, ToJSONKey, IsString)

instance Validity TimeZoneId

instance YamlKeySchema TimeZoneId where
  yamlKeySchema = TimeZoneId <$> yamlKeySchema

instance YamlSchema TimeZoneId where
  yamlSchema = TimeZoneId <$> yamlSchema
