{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.UnresolvedEvent where

import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.Set (Set)
import Data.Validity
import GHC.Generics
import Smos.Calendar.Import.Static
import Smos.Calendar.Import.TimeZone
import Smos.Calendar.Import.UnresolvedTimestamp
import YamlParse.Applicative

data UnresolvedEvents
  = UnresolvedEvents
      { unresolvedEventGroups :: [UnresolvedEventGroup],
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
      then toJSON unresolvedEventGroups
      else
        object
          [ "events" .= unresolvedEventGroups,
            "zones" .= unresolvedEventsTimeZones
          ]

data UnresolvedEventGroup
  = UnresolvedEventGroup
      { unresolvedEventGroupStatic :: !Static,
        unresolvedEvents :: !(Set UnresolvedEvent)
      }
  deriving (Show, Eq, Ord, Generic)

instance Validity UnresolvedEventGroup

instance YamlSchema UnresolvedEventGroup where
  yamlSchema =
    alternatives
      [ objectParser "UnresolvedEventGroup" $
          UnresolvedEventGroup
            <$> staticObjectParser
            <*> optionalFieldWithDefault' "events" S.empty,
        UnresolvedEventGroup emptyStatic <$> yamlSchema
      ]

instance FromJSON UnresolvedEventGroup where
  parseJSON = viaYamlSchema

instance ToJSON UnresolvedEventGroup where
  toJSON UnresolvedEventGroup {..} = case staticToObject unresolvedEventGroupStatic of
    [] -> toJSON unresolvedEvents
    ps ->
      object $
        ps
          ++ [ "events" .= unresolvedEvents
             ]

data UnresolvedEvent
  = UnresolvedEvent
      { unresolvedEventStart :: !(Maybe CalTimestamp),
        unresolvedEventEnd :: !(Maybe CalEndDuration)
      }
  deriving (Show, Eq, Ord, Generic)

instance Validity UnresolvedEvent

instance YamlSchema UnresolvedEvent where
  yamlSchema =
    objectParser "UnresolvedEvent" $
      UnresolvedEvent
        <$> optionalField' "start"
        <*> optionalField' "end"

instance FromJSON UnresolvedEvent where
  parseJSON = viaYamlSchema

instance ToJSON UnresolvedEvent where
  toJSON UnresolvedEvent {..} =
    object $
      concat
        [ ["start" .= s | s <- maybeToList unresolvedEventStart],
          ["end" .= e | e <- maybeToList unresolvedEventEnd]
        ]
