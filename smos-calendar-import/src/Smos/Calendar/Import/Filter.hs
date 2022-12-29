{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Filter where

import Control.Monad.Reader
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time
import GHC.Generics (Generic)
import qualified ICal.Component.TimeZone as ICal
import qualified ICal.Conformance as ICal
import qualified ICal.Parameter as ICal
import qualified ICal.Recurrence as ICal
import Smos.Calendar.Import.Event
import Smos.Calendar.Import.UnresolvedEvent
import Smos.Data

filterEventsSet :: Day -> Day -> Set Events -> Set Events
filterEventsSet start end = S.fromList . mapMaybe (filterEvents start end) . S.toList

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
