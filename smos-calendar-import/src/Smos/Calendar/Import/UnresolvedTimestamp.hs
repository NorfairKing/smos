{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Calendar.Import.UnresolvedTimestamp where

import Data.Aeson
import Data.Time
import Data.Validity
import GHC.Generics
import Smos.Calendar.Import.TimeZone
import Smos.Data
import YamlParse.Applicative

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
