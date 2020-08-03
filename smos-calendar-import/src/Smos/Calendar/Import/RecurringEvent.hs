{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.RecurringEvent where

import Data.Aeson
import Data.Text (Text)
import Data.Validity
import GHC.Generics
import Text.ICalendar.Types
import YamlParse.Applicative

data RecurringEvent
  = RecurringEvent
      { recurringEventSummary :: !(Maybe Text),
        recurringEventDescription :: !(Maybe Text)
      }
  deriving (Show, Eq, Generic)

instance Validity RecurringEvent

instance YamlSchema RecurringEvent where
  yamlSchema =
    objectParser "RecurringEvent" $
      RecurringEvent
        <$> optionalField' "summary"
        <*> optionalField' "description"

instance FromJSON RecurringEvent where
  parseJSON = viaYamlSchema

instance ToJSON RecurringEvent where
  toJSON RecurringEvent {..} =
    object
      [ "summary" .= recurringEventSummary,
        "description" .= recurringEventDescription
      ]
