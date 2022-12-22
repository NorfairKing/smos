{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Filter where

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time
import Smos.Calendar.Import.Event
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
