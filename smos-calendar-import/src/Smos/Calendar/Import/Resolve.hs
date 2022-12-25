{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Resolve where

import Control.Applicative
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time
import GHC.Generics (Generic)
import qualified ICal.Component.TimeZone as ICal
import qualified ICal.Conformance as ICal
import qualified ICal.Parameter as ICal
import qualified ICal.Property as ICal
import qualified ICal.PropertyType as ICal
import qualified ICal.Recurrence as ICal
import qualified ICal.Recurrence.Class as ICal
import Smos.Calendar.Import.Event
import Smos.Calendar.Import.UnresolvedEvent
import Smos.Data

resolveEvents :: Day -> Day -> TimeZone -> Set UnresolvedEvents -> Set Events
resolveEvents start end tz = S.unions . map (resolveUnresolvedEvents start end tz) . S.toList

resolveUnresolvedEvents :: Day -> Day -> TimeZone -> UnresolvedEvents -> Set Events
resolveUnresolvedEvents start end tz UnresolvedEvents {..} =
  let ctx =
        RecurCtx
          { resolveCtxTimeZone = tz,
            resolveCtxTimeZones = unresolvedEventsTimeZones
          }
   in S.fromList $
        flip mapMaybe (S.toList unresolvedEventGroups) $ \eg ->
          let errOrEvents = ICal.runConform $ runReaderT (resolveEventGroup eg) ctx
           in case errOrEvents of
                Left _ -> Nothing
                Right (events, _) -> filterEvents start end events

filterEvents :: Day -> Day -> Events -> Maybe Events
filterEvents start end e@Events {..} =
  let s = S.filter (filterEvent start end) events
   in if S.null s then Nothing else Just $ e {events = s}

filterEvent :: Day -> Day -> Event -> Bool
filterEvent lo hi Event {..} = case (eventStart, eventEnd) of
  (Nothing, Nothing) -> True
  (Just start, Nothing) -> timestampDay start <= hi
  (Nothing, Just end) -> lo <= timestampDay end
  (Just start, Just end) ->
    timestampDay start <= hi
      && lo <= timestampDay end

data RecurCtx = RecurCtx
  { resolveCtxTimeZone :: TimeZone,
    resolveCtxTimeZones :: Map ICal.TZIDParam ICal.TimeZone
  }
  deriving (Show, Eq, Generic)

type R a = ReaderT RecurCtx ICal.Resolv a

resolveEventGroup :: UnresolvedEventGroup -> R Events
resolveEventGroup UnresolvedEventGroup {..} = do
  let eventsStatic = unresolvedEventGroupStatic
  events <- S.fromList <$> mapM resolveEvent (S.toList unresolvedEvents)
  pure Events {..}

resolveEvent :: ICal.EventOccurrence -> R Event
resolveEvent eo = do
  myZone <- asks resolveCtxTimeZone
  zones <- asks resolveCtxTimeZones
  ICal.ResolvedEvent {..} <- lift $ ICal.runR zones $ ICal.resolveEventOccurrence myZone eo
  let eventStart = either TimestampDay TimestampLocalTime <$> resolvedEventStart
  let eventEnd =
        case either TimestampDay TimestampLocalTime <$> resolvedEventEnd of
          Just e -> Just e
          -- Use the event start so we definitely have an endpoint. This is the way google calendar does it.
          -- This is important because otherwise very old events without an end time are always imported.
          Nothing -> eventStart
  pure Event {..}
