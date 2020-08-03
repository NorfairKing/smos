{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Event where

import Control.Applicative
import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import Data.Validity
import Data.Validity.Text
import GHC.Generics (Generic)
import Smos.Data
import YamlParse.Applicative

data Event
  = Event
      { eventSummary :: !(Maybe Text),
        eventDescription :: !(Maybe Text),
        eventStart :: !(Maybe Timestamp),
        eventEnd :: !(Maybe Timestamp)
      }
  deriving (Show, Eq, Generic)

instance Validity Event where
  validate e@Event {..} =
    mconcat
      [ genericValidate e,
        decorate "The title is a single line if it exists" $ maybe valid validateTextSingleLine eventSummary,
        declare "The end happens before the start"
          $ fromMaybe True
          $ liftA2 (>=) eventStart eventEnd
      ]

instance YamlSchema Event where
  yamlSchema =
    objectParser "Event" $
      Event
        <$> optionalField' "summary"
        <*> optionalField' "description"
        <*> optionalField' "start"
        <*> optionalField' "end"

instance FromJSON Event where
  parseJSON = viaYamlSchema

instance ToJSON Event where
  toJSON Event {..} =
    object
      [ "summary" .= eventSummary,
        "description" .= eventDescription,
        "start" .= eventStart,
        "end" .= eventEnd
      ]
