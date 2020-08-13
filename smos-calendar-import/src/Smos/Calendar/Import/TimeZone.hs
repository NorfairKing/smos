{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.TimeZone where

import Data.Aeson
import qualified Data.Set as S
import Data.Set (Set)
import Data.String
import Data.Text (Text)
import Data.Time
import Data.Validity
import GHC.Generics
import Smos.Calendar.Import.RecurrenceRule
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
  deriving (Show, Eq, Generic)

instance Validity TimeZoneHistory

instance YamlSchema TimeZoneHistory where
  yamlSchema =
    TimeZoneHistory
      <$> alternatives
        [ yamlSchema,
          (: []) <$> yamlSchema
        ]

instance FromJSON TimeZoneHistory where
  parseJSON = viaYamlSchema

instance ToJSON TimeZoneHistory where
  toJSON (TimeZoneHistory [x]) = toJSON x
  toJSON (TimeZoneHistory l) = toJSON l

data TimeZoneHistoryRule
  = TimeZoneHistoryRule
      { timeZoneHistoryRuleStart :: !LocalTime, -- In the timezone at the time
        timeZoneHistoryRuleOffsetFrom :: !UTCOffset,
        timeZoneHistoryRuleOffsetTo :: !UTCOffset,
        timeZoneHistoryRuleRRules :: !(Set RRule)
      }
  deriving (Show, Eq, Ord, Generic)

instance Validity TimeZoneHistoryRule

instance YamlSchema TimeZoneHistoryRule where
  yamlSchema =
    objectParser "TimeZoneHistoryRule" $
      TimeZoneHistoryRule
        <$> requiredFieldWith' "start" localTimeSchema
        <*> requiredField' "from"
        <*> requiredField' "to"
        <*> optionalFieldWithDefault' "rules" S.empty

instance FromJSON TimeZoneHistoryRule where
  parseJSON = viaYamlSchema

instance ToJSON TimeZoneHistoryRule where
  toJSON TimeZoneHistoryRule {..} =
    object $
      concat
        [ [ "start" .= formatTime defaultTimeLocale timestampLocalTimeFormat timeZoneHistoryRuleStart,
            "from" .= timeZoneHistoryRuleOffsetFrom,
            "to" .= timeZoneHistoryRuleOffsetTo
          ],
          [ "rules" .= timeZoneHistoryRuleRRules | not (S.null timeZoneHistoryRuleRRules)
          ]
        ]

newtype UTCOffset = UTCOffset Int -- Minutes from UTCTime
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

instance Validity UTCOffset

instance YamlSchema UTCOffset where
  yamlSchema = UTCOffset <$> yamlSchema
