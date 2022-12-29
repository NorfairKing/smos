{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Calendar.Import.UTCEvent where

import Autodocodec
import Control.Applicative
import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Validity
import GHC.Generics (Generic)
import ICal.Extended ()
import qualified ICal.Recurrence as ICal
import Smos.Calendar.Import.Static
import Smos.Data

-- A collection of events from the same recurrence set
data UTCEvents = UTCEvents
  { utcEventsStatic :: !Static,
    utcEvents :: !(Set UTCEvent)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec UTCEvents)

instance Validity UTCEvents

instance HasCodec UTCEvents where
  codec =
    dimapCodec f g $
      eitherCodec
        ( object "UTCEvents" $
            UTCEvents
              <$> objectCodec .= utcEventsStatic
              <*> optionalFieldWithOmittedDefault' "events" S.empty .= utcEvents
        )
        codec
    where
      f = \case
        Left e -> e
        Right es -> UTCEvents emptyStatic es
      g es =
        if utcEventsStatic es == emptyStatic
          then Right (utcEvents es)
          else Left es

data UTCEvent = UTCEvent
  { utcEventStart :: !(Maybe ICal.Timestamp),
    utcEventEnd :: !(Maybe ICal.Timestamp)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec UTCEvent)

instance Validity UTCEvent

instance HasCodec UTCEvent where
  codec =
    object "UTCEvent" $
      UTCEvent
        <$> optionalField' "start" .= utcEventStart
        <*> optionalField' "end" .= utcEventEnd
