{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.TimeZone where

import Data.Aeson
import Data.Text (Text)
import Data.Time
import Data.Validity
import GHC.Generics
import Smos.Data
import YamlParse.Applicative

newtype TimeZoneId = TimeZoneId Text -- Unique id of the timezone
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

newtype UTCOffset = UTCOffset Int -- Minutes from UTCTime
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance Validity UTCOffset

instance YamlSchema UTCOffset where
  yamlSchema = UTCOffset <$> yamlSchema
