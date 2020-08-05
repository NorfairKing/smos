{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.TimeZone where

import Data.Aeson
import Data.String
import Data.Text (Text)
import Data.Time
import Data.Validity
import GHC.Generics
import Smos.Data
import YamlParse.Applicative

newtype TimeZoneId = TimeZoneId Text -- Unique id of the timezone
  deriving (Show, Eq, Ord, Generic, FromJSON, FromJSONKey, ToJSON, ToJSONKey, IsString)

instance Validity TimeZoneId

instance YamlKeySchema TimeZoneId where
  yamlKeySchema = TimeZoneId <$> yamlKeySchema

instance YamlSchema TimeZoneId where
  yamlSchema = TimeZoneId <$> yamlSchema

newtype TimeZoneHistory = TimeZoneHistory {timeZoneHistoryRules :: [TimeZoneHistoryRule]}
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance Validity TimeZoneHistory

instance YamlSchema TimeZoneHistory where
  yamlSchema =
    TimeZoneHistory
      <$> alternatives
        [ yamlSchema,
          (: []) <$> yamlSchema
        ]

data TimeZoneHistoryRule
  = TimeZoneHistoryRule
      { timeZoneHistoryRuleStart :: LocalTime, -- In the timezone at the time
        timeZoneHistoryRuleOffsetFrom :: UTCOffset,
        timeZoneHistoryRuleOffsetTo :: UTCOffset
      }
  deriving (Show, Eq, Generic)

instance Validity TimeZoneHistoryRule

instance YamlSchema TimeZoneHistoryRule where
  yamlSchema =
    objectParser "TimeZoneHistoryRule" $
      TimeZoneHistoryRule
        <$> requiredFieldWith' "start" localTimeSchema
        <*> requiredField' "from"
        <*> requiredField' "to"

instance FromJSON TimeZoneHistoryRule where
  parseJSON = viaYamlSchema

instance ToJSON TimeZoneHistoryRule where
  toJSON TimeZoneHistoryRule {..} =
    object
      [ "start" .= formatTime defaultTimeLocale timestampLocalTimeFormat timeZoneHistoryRuleStart,
        "from" .= timeZoneHistoryRuleOffsetFrom,
        "to" .= timeZoneHistoryRuleOffsetTo
      ]

newtype UTCOffset = UTCOffset Int -- Minutes from UTCTime
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance Validity UTCOffset

instance YamlSchema UTCOffset where
  yamlSchema = UTCOffset <$> yamlSchema
