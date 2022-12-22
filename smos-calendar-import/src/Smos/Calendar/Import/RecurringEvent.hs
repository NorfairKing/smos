{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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
import qualified ICal.Component.TimeZone as ICal
import qualified ICal.Property as ICal
import qualified ICal.Recurrence as ICal
import Smos.Calendar.Import.Static
import Smos.Calendar.Import.UnresolvedTimestamp

data RecurringEvents = RecurringEvents
  { recurringEvents :: Map Text (Set RecurringEvent),
    recurringEventsTimeZones :: Map ICal.TZID ICal.TimeZone
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
    recurringEventRecurrence :: !ICal.Recurrence
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec RecurringEvent)

instance Validity RecurringEvent

instance HasCodec RecurringEvent where
  codec =
    named "RecurringEvent" $
      object "RecurringEvent" $
        RecurringEvent
          <$> objectCodec .= recurringEventStatic
          <*> optionalFieldOrNull' "start" .= recurringEventStart
          <*> optionalFieldOrNull' "end" .= recurringEventEnd
          <*> objectCodec .= recurringEventRecurrence

instance HasObjectCodec ICal.Recurrence where
  objectCodec =
    ICal.Recurrence
      <$> optionalFieldOrNullWithOmittedDefault' "rrule" S.empty .= ICal.recurrenceRules
      <*> optionalFieldOrNullWithOmittedDefault' "exceptions" S.empty .= ICal.recurrenceExceptions
      <*> optionalFieldOrNullWithOmittedDefault' "rdates" S.empty .= ICal.recurrenceRDates
