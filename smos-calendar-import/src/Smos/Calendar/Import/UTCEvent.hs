{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Calendar.Import.UTCEvent where

import Autodocodec
import Data.Set (Set)
import qualified Data.Set as S
import ICal.Extended ()
import qualified ICal.Recurrence as ICal
import Smos.Calendar.Import.Static

-- A collection of events from the same recurrence set
data UTCEvents = UTCEvents
  { utcEventsStatic :: !Static,
    utcEvents :: !(Set UTCEvent)
  }
  deriving stock (Show, Eq, Ord)

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
  deriving stock (Show, Eq, Ord)

instance HasCodec UTCEvent where
  codec =
    object "UTCEvent" $
      UTCEvent
        <$> optionalField' "start" .= utcEventStart
        <*> optionalField' "end" .= utcEventEnd
