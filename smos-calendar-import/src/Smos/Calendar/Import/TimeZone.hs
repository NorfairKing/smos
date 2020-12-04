{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.TimeZone where

import Data.Aeson
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time
import Data.Validity
import GHC.Generics
import Smos.Calendar.Import.RecurrenceRule
import Smos.Calendar.Import.UnresolvedTimestamp
import Smos.Data
import YamlParse.Applicative

newtype TimeZoneHistory = TimeZoneHistory {timeZoneHistoryRules :: [TimeZoneHistoryRule]}
  deriving (Show, Eq, Ord, Generic)

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

data TimeZoneHistoryRule = TimeZoneHistoryRule
  { timeZoneHistoryRuleStart :: !LocalTime, -- In the timezone at the time
    timeZoneHistoryRuleOffsetFrom :: !UTCOffset,
    timeZoneHistoryRuleOffsetTo :: !UTCOffset,
    timeZoneHistoryRuleRRules :: !(Set RRule),
    timeZoneHistoryRuleRDates :: !(Set CalRDate)
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
        <*> optionalFieldWithDefault' "rdates" S.empty

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
          ],
          [ "rdates" .= timeZoneHistoryRuleRDates | not (S.null timeZoneHistoryRuleRDates)
          ]
        ]

newtype UTCOffset = UTCOffset Int -- Minutes from UTCTime
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

instance Validity UTCOffset

instance YamlSchema UTCOffset where
  yamlSchema = UTCOffset <$> yamlSchema
