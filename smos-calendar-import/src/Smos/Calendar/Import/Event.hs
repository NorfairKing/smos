{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Event where

import Control.Applicative
import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import Data.Validity
import GHC.Generics (Generic)
import Smos.Calendar.Import.Static
import Smos.Data
import YamlParse.Applicative

-- A collection of events from the same recurrence set
data Events
  = Events
      { eventsTitle :: !(Maybe Text),
        events :: ![Event]
      }
  deriving (Show, Eq, Generic)

instance Validity Events

instance YamlSchema Events where
  yamlSchema =
    alternatives
      [ objectParser "Events" $
          Events
            <$> optionalField' "title"
            <*> optionalFieldWithDefault' "events" [],
        Events Nothing <$> yamlSchema
      ]

instance FromJSON Events where
  parseJSON = viaYamlSchema

instance ToJSON Events where
  toJSON Events {..} = case eventsTitle of
    Nothing -> toJSON events
    Just title ->
      object
        [ "title" .= title,
          "events" .= events
        ]

data Event
  = Event
      { eventStatic :: !Static,
        eventStart :: !(Maybe Timestamp),
        eventEnd :: !(Maybe Timestamp)
      }
  deriving (Show, Eq, Generic)

instance Validity Event where
  validate e@Event {..} =
    mconcat
      [ genericValidate e,
        declare "The end happens before the start"
          $ fromMaybe True
          $ liftA2 (>=) eventStart eventEnd
      ]

instance YamlSchema Event where
  yamlSchema =
    objectParser "Event" $
      Event
        <$> staticObjectParser
        <*> optionalField' "start"
        <*> optionalField' "end"

instance FromJSON Event where
  parseJSON = viaYamlSchema

instance ToJSON Event where
  toJSON Event {..} =
    object $
      staticToObject eventStatic
        ++ concat
          [ ["start" .= s | s <- maybeToList eventStart],
            ["end" .= e | e <- maybeToList eventEnd]
          ]
