{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Event where

import Autodocodec
import Control.Applicative
import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Validity
import GHC.Generics (Generic)
import Smos.Calendar.Import.Static
import Smos.Data

-- A collection of events from the same recurrence set
data Events = Events
  { eventsStatic :: !Static,
    events :: !(Set Event)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Events)

instance Validity Events

instance HasCodec Events where
  codec =
    dimapCodec f g $
      eitherCodec
        ( object "Events" $
            Events
              <$> staticObjectCodec .= eventsStatic
              <*> optionalFieldWithOmittedDefault' "events" S.empty .= events
        )
        codec
    where
      f = \case
        Left e -> e
        Right es -> Events emptyStatic es
      g es =
        if eventsStatic es == emptyStatic
          then Right (events es)
          else Left es

data Event = Event
  { eventStart :: !(Maybe Timestamp),
    eventEnd :: !(Maybe Timestamp)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Event)

instance Validity Event where
  validate e@Event {..} =
    mconcat
      [ genericValidate e,
        declare "The end happens before the start" $
          fromMaybe True $
            liftA2 (>=) eventStart eventEnd
      ]

instance HasCodec Event where
  codec =
    object "Event" $
      Event
        <$> optionalField' "start" .= eventStart
        <*> optionalField' "end" .= eventEnd
