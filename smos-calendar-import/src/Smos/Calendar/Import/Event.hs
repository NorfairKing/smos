{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Event where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text
import GHC.Generics (Generic)
import YamlParse.Applicative

data Event
  = Event
      { eventSummary :: !(Maybe Text),
        eventDescription :: !(Maybe Text)
      }
  deriving (Show, Eq, Generic)

instance Validity Event where
  validate e@Event {..} =
    mconcat
      [ genericValidate e,
        decorate "The title is a single line if it exists" $ maybe valid validateTextSingleLine eventSummary
      ]

instance YamlSchema Event where
  yamlSchema =
    objectParser "Event" $
      Event
        <$> optionalField' "summary"
        <*> optionalField' "description"

instance FromJSON Event where
  parseJSON = viaYamlSchema

instance ToJSON Event where
  toJSON Event {..} =
    object
      [ "summary" .= eventSummary,
        "description" .= eventDescription
      ]
