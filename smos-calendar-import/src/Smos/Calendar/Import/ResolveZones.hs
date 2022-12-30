{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.ResolveZones (resolveUnresolvedEvents) where

import Data.Set (Set)
import qualified Data.Set as S
import qualified ICal.Recurrence as ICal
import Smos.Calendar.Import.UTCEvent
import Smos.Calendar.Import.UnresolvedEvent

resolveUnresolvedEvents :: UnresolvedEvents -> ICal.R (Set UTCEvents)
resolveUnresolvedEvents = fmap S.fromList . mapM resolveUnresolvedEventGroup . S.toList . unresolvedEventGroups

resolveUnresolvedEventGroup :: UnresolvedEventGroup -> ICal.R UTCEvents
resolveUnresolvedEventGroup UnresolvedEventGroup {..} = do
  let utcEventsStatic = unresolvedEventGroupStatic
  utcEvents <- S.fromList <$> mapM resolveEventOccurrence (S.toList unresolvedEvents)
  pure UTCEvents {..}

resolveEventOccurrence :: ICal.EventOccurrence -> ICal.R UTCEvent
resolveEventOccurrence eo = do
  ICal.ResolvedEvent {..} <- ICal.resolveEventOccurrence eo
  let utcEventStart = resolvedEventStart
  let utcEventEnd =
        case resolvedEventEnd of
          Just e -> Just e
          -- Use the event start so we definitely have an endpoint. This is the way google calendar does it.
          -- This is important because otherwise very old events without an end time are always imported.
          Nothing -> utcEventStart
  pure UTCEvent {..}
