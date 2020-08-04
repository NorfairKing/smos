{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Event where

import Control.Applicative
import Data.Aeson
import Data.Maybe
import Data.Validity
import GHC.Generics (Generic)
import Smos.Calendar.Import.Static
import Smos.Data
import YamlParse.Applicative

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
        ++ [ "start" .= eventStart,
             "end" .= eventEnd
           ]
