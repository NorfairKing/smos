{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.RecurringEvent where

import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set (Set)
import Data.Validity
import GHC.Generics
import Smos.Calendar.Import.RecurrenceRule
import Smos.Calendar.Import.Static
import Smos.Calendar.Import.TimeZone
import Smos.Calendar.Import.UnresolvedTimestamp
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
        recurringEventEnd :: !(Maybe CalEndDuration),
        recurringEventRRules :: !(Set RRule)
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
        <*> optionalFieldWithDefault' "rrule" S.empty

instance FromJSON RecurringEvent where
  parseJSON = viaYamlSchema

instance ToJSON RecurringEvent where
  toJSON RecurringEvent {..} =
    object $
      staticToObject recurringEventStatic
        ++ [ "start" .= recurringEventStart,
             "end" .= recurringEventEnd,
             "rrule" .= recurringEventRRules
           ]
