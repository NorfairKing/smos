{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Event where

import Control.Applicative
import Data.Aeson
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Validity
import GHC.Generics (Generic)
import Smos.Calendar.Import.Static
import Smos.Data
import YamlParse.Applicative

-- A collection of events from the same recurrence set
data Events = Events
  { eventsStatic :: !Static,
    events :: !(Set Event)
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity Events

instance YamlSchema Events where
  yamlSchema =
    alternatives
      [ objectParser "Events" $
          Events
            <$> staticObjectParser
            <*> optionalFieldWithDefault' "events" S.empty,
        Events emptyStatic <$> yamlSchema
      ]

instance FromJSON Events where
  parseJSON = viaYamlSchema

instance ToJSON Events where
  toJSON Events {..} = case staticToObject eventsStatic of
    [] -> toJSON events
    sps ->
      object $
        sps
          ++ [ "events" .= events
             ]

data Event = Event
  { eventStart :: !(Maybe Timestamp),
    eventEnd :: !(Maybe Timestamp)
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity Event where
  validate e@Event {..} =
    mconcat
      [ genericValidate e,
        declare "The end happens before the start" $
          fromMaybe True $
            liftA2 (>=) eventStart eventEnd
      ]

instance YamlSchema Event where
  yamlSchema =
    objectParser "Event" $
      Event
        <$> optionalField' "start"
        <*> optionalField' "end"

instance FromJSON Event where
  parseJSON = viaYamlSchema

instance ToJSON Event where
  toJSON Event {..} =
    object $
      concat
        [ ["start" .= s | s <- maybeToList eventStart],
          ["end" .= e | e <- maybeToList eventEnd]
        ]
