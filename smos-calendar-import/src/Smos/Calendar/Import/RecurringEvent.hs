{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.RecurringEvent where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import GHC.Generics
import Smos.Calendar.Import.RecurrenceRule
import Smos.Calendar.Import.Static
import Smos.Calendar.Import.TimeZone
import Smos.Calendar.Import.UnresolvedTimestamp

data RecurringEvents = RecurringEvents
  { recurringEvents :: Map Text (Set RecurringEvent),
    recurringEventsTimeZones :: Map TimeZoneId TimeZoneHistory
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec RecurringEvents)

instance Validity RecurringEvents

-- TODO validity constraints on timezone ids

instance HasCodec RecurringEvents where
  codec =
    dimapCodec f1 g1 $
      eitherCodec
        ( object "RecurringEvents" $
            RecurringEvents
              <$> requiredFieldWith' "events" eventsCodec .= recurringEvents
              <*> optionalFieldWithOmittedDefault' "zones" M.empty .= recurringEventsTimeZones
        )
        eventsCodec
    where
      f1 = \case
        Left res -> res
        Right es -> RecurringEvents es M.empty
      g1 res =
        if null (recurringEventsTimeZones res)
          then Right (recurringEvents res)
          else Left res
      eventsCodec :: JSONCodec (Map Text (Set RecurringEvent))
      eventsCodec = dimapCodec f2 g2 $ eitherCodec codec codec
        where
          f2 = \case
            Left m -> m
            Right is -> M.fromList $ zipWith (\i e -> (T.pack (show (i :: Word)), S.singleton e)) [0 ..] is
          g2 = Left -- TODO if it's just numbered ones, serialise them as a list?

data RecurringEvent = RecurringEvent
  { recurringEventStatic :: !Static,
    recurringEventStart :: !(Maybe CalTimestamp),
    recurringEventEnd :: !(Maybe CalEndDuration),
    recurringEventRecurrence :: !Recurrence
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity RecurringEvent

instance HasCodec RecurringEvent where
  codec =
    object "RecurringEvent" $
      RecurringEvent
        <$> staticObjectCodec .= recurringEventStatic
        <*> optionalField' "start" .= recurringEventStart
        <*> optionalField' "end" .= recurringEventEnd
        <*> recurrenceObjectCodec .= recurringEventRecurrence

data Recurrence = Recurrence
  { -- | We use a set here instead of a Maybe because the spec says:
    --
    -- >  ; The following is OPTIONAL,
    -- >  ; but SHOULD NOT occur more than once.
    -- >  ;
    -- >  rrule /
    --
    -- It says "SHOULD NOT" instead of "MUST NOT" so we are opting to support it.
    --
    -- It also says "The recurrence set generated with multiple "RRULE" properties is undefined."
    -- so we choose to define it as the union of the recurrence sets defined by the rules.
    recurrenceRules :: !(Set RRule),
    recurrenceExceptions :: !(Set CalTimestamp),
    recurrenceRDates :: !(Set CalRDate)
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity Recurrence

instance HasCodec Recurrence where
  codec = object "Recurrence" recurrenceObjectCodec

emptyRecurrence :: Recurrence
emptyRecurrence =
  Recurrence
    { recurrenceRules = S.empty,
      recurrenceExceptions = S.empty,
      recurrenceRDates = S.empty
    }

recurrenceObjectCodec :: ObjectCodec Recurrence Recurrence
recurrenceObjectCodec =
  Recurrence
    <$> optionalFieldWithOmittedDefault' "rrule" S.empty .= recurrenceRules
    <*> optionalFieldWithOmittedDefault' "exceptions" S.empty .= recurrenceExceptions
    <*> optionalFieldWithOmittedDefault' "rdates" S.empty .= recurrenceRDates
