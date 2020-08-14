{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.RecurringEvent where

import Data.Aeson
import Data.Aeson.Types
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
        recurringEventRecurrence :: !Recurrence
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
        <*> recurrenceObjectParser

instance FromJSON RecurringEvent where
  parseJSON = viaYamlSchema

instance ToJSON RecurringEvent where
  toJSON RecurringEvent {..} =
    object $
      staticToObject recurringEventStatic
        ++ concat
          [ [ "start" .= recurringEventStart,
              "end" .= recurringEventEnd
            ],
            recurrenceToObject recurringEventRecurrence
          ]

data Recurrence
  = Recurrence
      { -- | We use a set here instead of a Maybe because the spec says:
        --
        -- >  ; The following is OPTIONAL,
        -- >  ; but SHOULD NOT occur more than once.
        -- >  ;
        -- >  rrule /
        --
        -- It says "SHOULD NOT" instead of "MUST NOT" so we are opting to support it.
        --
        -- It also says "The recurrence set generated with multiple "RRULE" properties is undefined."
        -- so we choose to define it as the union of the recurrence sets defined by the rules.
        recurrenceRules :: !(Set RRule),
        recurrenceExceptions :: !(Set CalTimestamp)
      }
  deriving (Show, Eq, Generic)

instance Validity Recurrence

instance YamlSchema Recurrence where
  yamlSchema = objectParser "Recurrence" recurrenceObjectParser

emptyRecurrence :: Recurrence
emptyRecurrence =
  Recurrence
    { recurrenceRules = S.empty,
      recurrenceExceptions = S.empty
    }

instance FromJSON Recurrence where
  parseJSON = viaYamlSchema

instance ToJSON Recurrence where
  toJSON = object . recurrenceToObject

recurrenceObjectParser :: ObjectParser Recurrence
recurrenceObjectParser =
  Recurrence
    <$> optionalFieldWithDefault' "rrule" S.empty
    <*> optionalFieldWithDefault' "exceptions" S.empty

recurrenceToObject :: Recurrence -> [Pair]
recurrenceToObject Recurrence {..} =
  concat
    [ ["rrule" .= recurrenceRules | not (S.null recurrenceRules)],
      ["exceptions" .= recurrenceExceptions | not (S.null recurrenceExceptions)]
    ]
