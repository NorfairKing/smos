{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.UnresolvedEvent where

import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as M
import Data.Validity
import GHC.Generics
import Smos.Calendar.Import.Static
import Smos.Calendar.Import.TimeZone
import Smos.Calendar.Import.UnresolvedTimestamp
import YamlParse.Applicative

data UnresolvedEvents
  = UnresolvedEvents
      { unresolvedEvents :: [UnresolvedEvent],
        unresolvedEventsTimeZones :: Map TimeZoneId TimeZoneHistory
      }
  deriving (Show, Eq, Generic)

instance Validity UnresolvedEvents

-- TODO validity constraints on timezone ids

instance YamlSchema UnresolvedEvents where
  yamlSchema =
    alternatives
      [ objectParser "UnresolvedEvents" $
          UnresolvedEvents
            <$> requiredField' "events"
            <*> optionalFieldWithDefault' "zones" M.empty,
        UnresolvedEvents <$> yamlSchema <*> pure M.empty
      ]

instance FromJSON UnresolvedEvents where
  parseJSON = viaYamlSchema

instance ToJSON UnresolvedEvents where
  toJSON UnresolvedEvents {..} =
    if M.null unresolvedEventsTimeZones
      then toJSON unresolvedEvents
      else object ["events" .= unresolvedEvents, "zones" .= unresolvedEventsTimeZones]

data UnresolvedEvent
  = UnresolvedEvent
      { unresolvedEventStatic :: !Static,
        unresolvedEventStart :: !(Maybe CalTimestamp),
        unresolvedEventEnd :: !(Maybe CalEndDuration)
      }
  deriving (Show, Eq, Generic)

instance Validity UnresolvedEvent

instance YamlSchema UnresolvedEvent where
  yamlSchema =
    objectParser "UnresolvedEvent" $
      UnresolvedEvent
        <$> staticObjectParser
        <*> optionalField' "start"
        <*> optionalField' "end"

instance FromJSON UnresolvedEvent where
  parseJSON = viaYamlSchema

instance ToJSON UnresolvedEvent where
  toJSON UnresolvedEvent {..} =
    object $
      staticToObject unresolvedEventStatic
        ++ [ "start" .= unresolvedEventStart,
             "end" .= unresolvedEventEnd
           ]
