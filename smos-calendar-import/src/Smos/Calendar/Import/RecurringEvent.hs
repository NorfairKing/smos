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
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Validity
import GHC.Generics
import ICal.Extended
import qualified ICal.Property as ICal
import qualified ICal.PropertyType as ICal
import qualified ICal.Recurrence as ICal
import Smos.Calendar.Import.Static
import Text.Read

newtype RecurringEvents = RecurringEvents {recurringEvents :: Map ICal.UID (Set RecurringEvent)}
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec RecurringEvents)

instance Validity RecurringEvents

-- TODO validity constraints on timezone ids

instance HasCodec RecurringEvents where
  codec = dimapCodec RecurringEvents recurringEvents eventsCodec
    where
      eventsCodec :: JSONCodec (Map ICal.UID (Set RecurringEvent))
      eventsCodec = dimapCodec f2 g2 $ eitherCodec codec codec
        where
          f2 ::
            Either (Map ICal.UID (Set RecurringEvent)) [RecurringEvent] ->
            Map ICal.UID (Set RecurringEvent)
          f2 = \case
            Left m -> m
            Right is -> M.fromList $ zipWith (\i e -> (ICal.UID (T.pack (show (i :: Word))), S.singleton e)) [0 ..] is
          g2 ::
            Map ICal.UID (Set RecurringEvent) ->
            Either (Map ICal.UID (Set RecurringEvent)) [RecurringEvent]
          g2 m =
            let tups = M.toList m
                mSingles =
                  map
                    ( \(uid, s) -> case S.toList s of
                        [v] -> case (readMaybe :: String -> Maybe Word) (T.unpack (ICal.unUID uid)) of
                          Just _ -> Just v
                          Nothing -> Nothing
                        _ -> Nothing
                    )
                    tups
             in if all isJust mSingles
                  then Right $ catMaybes mSingles
                  else Left m

-- | Whether a component's instances belong in the imported calendar
--
-- This is decided while picking but cannot be acted on there.  A component with
-- STATUS:CANCELLED and a RECURRENCE-ID is how "delete this one instance" is
-- written, so dropping such a component before the recurrence set is
-- reconciled makes the series' instance reappear at that time instead of
-- disappearing.  SMOS_NO_CALENDAR_IMPORT is a per-component opt-out and an
-- overriding component has a description of its own, so it has the same
-- problem.
data Inclusion = Include | Exclude
  deriving (Show, Eq, Ord, Generic)

instance Validity Inclusion

instance HasCodec Inclusion where
  codec = dimapCodec f g codec
    where
      f = \case
        True -> Include
        False -> Exclude
      g = \case
        Include -> True
        Exclude -> False

-- | One picked component of a recurrence set
--
-- This is everything recurrence needs from a component, plus what smos keeps of
-- it, and it is what the picked form on disk stores.  There is no UID field
-- because 'RecurringEvents' keys the map by it; 'recurringOf' puts the two back
-- together.
data RecurringEvent = RecurringEvent
  { recurringEventStatic :: !Static,
    recurringEventInclusion :: !Inclusion,
    recurringEventSequenceNumber :: !ICal.SequenceNumber,
    recurringEventRecurrenceIdentifier :: !(Maybe ICal.RecurrenceIdentifier),
    recurringEventStart :: !(Maybe ICal.DateTimeStart),
    recurringEventEndOrDuration :: !(Maybe (Either ICal.RecurrenceEnd ICal.Duration)),
    recurringEventRecurrence :: !ICal.Recurrence
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity RecurringEvent

instance HasCodec RecurringEvent where
  codec =
    named "RecurringEvent" $
      object "RecurringEvent" $
        RecurringEvent
          <$> objectCodec .= recurringEventStatic
          <*> optionalFieldWithOmittedDefault "include" Include "whether to import this component's instances"
            .= recurringEventInclusion
          <*> optionalFieldWithOmittedDefault "sequence" ICal.defaultSequenceNumber "which revision of this component this is"
            .= recurringEventSequenceNumber
          <*> optionalField "recurrence-id" "the instance of the series that this component overrides"
            .= recurringEventRecurrenceIdentifier
          <*> optionalField "dtstart" "start date time"
            .= recurringEventStart
          <*> endDurationObjectCodec
            .= recurringEventEndOrDuration
          <*> recurrenceObjectCodec
            .= recurringEventRecurrence

-- | Everything that one UID's components contribute to its recurrence set
--
-- The occurrences of one component depend on the others that share its UID,
-- which is why recurrence takes the whole group.
-- The component each instance carries is the inclusion flag alongside the
-- 'Static', because 'Recur' needs both after reconciliation: the flag to drop
-- what was never meant to be imported, and the 'Static' to group by.
recurringOf :: RecurringEvents -> [ICal.Recurring (Inclusion, Static)]
recurringOf RecurringEvents {..} =
  [ ICal.Recurring
      { ICal.recurringComponent = (recurringEventInclusion re, recurringEventStatic re),
        ICal.recurringUID = uid,
        ICal.recurringSequenceNumber = recurringEventSequenceNumber re,
        ICal.recurringRecurrenceIdentifier = recurringEventRecurrenceIdentifier re,
        ICal.recurringStart = recurringEventStart re,
        ICal.recurringEnd = recurringEventEndOrDuration re,
        ICal.recurringRecurrence = recurringEventRecurrence re
      }
  | (uid, res) <- M.toList recurringEvents,
    re <- S.toList res
  ]
