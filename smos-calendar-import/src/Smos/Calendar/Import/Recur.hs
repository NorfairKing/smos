{-# LANGUAGE RecordWildCards #-}

-- This module uses list as a monad a lot, make sure you understand it before reading this module.
module Smos.Calendar.Import.Recur (recurRecurringEvents) where

import Control.Monad.Reader
import Data.List
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time
import qualified ICal.Recurrence as ICal
import Smos.Calendar.Import.RecurringEvent
import Smos.Calendar.Import.UnresolvedEvent

recurRecurringEvents :: Day -> RecurringEvents -> ICal.R UnresolvedEvents
recurRecurringEvents limit RecurringEvents {..} = do
  unresolvedEventGroups <- fmap (deduplicateBasedOnId . S.unions) $
    forM (M.toList recurringEvents) $ \(_, es) ->
      recurEventSet limit es
  pure UnresolvedEvents {..}

recurEventSet :: Day -> Set RecurringEvent -> ICal.R (Set UnresolvedEventGroup)
recurEventSet limit = fmap S.fromList . mapM (recurEvent limit) . S.toList

recurEvent :: Day -> RecurringEvent -> ICal.R UnresolvedEventGroup
recurEvent limit RecurringEvent {..} = do
  let unresolvedEventGroupStatic = recurringEventStatic
  unresolvedEvents <- ICal.recurEvents limit recurringEventEvent
  let unresolvedEventGroupRecurrenceId = recurringEventRecurrenceId
  pure UnresolvedEventGroup {..}

deduplicateBasedOnId :: Set UnresolvedEventGroup -> Set UnresolvedEventGroup
deduplicateBasedOnId = snd . foldl' goGroup (S.empty, S.empty) . toSortedList
  where
    -- Sort groups by their descending order of their first occurrence.
    -- The reasoning is that this marks the original version of the event.
    -- Any changed occurrences that may produce duplicates will have been introduced later.
    toSortedList :: Set UnresolvedEventGroup -> [UnresolvedEventGroup]
    toSortedList = sortOn (S.toDescList . unresolvedEvents) . S.toList
    -- Go through all groups.
    -- For each group we only keep the events that are not in any other group.
    -- We will keep the results in the second element of the tuple, and the total collection of the events in the first element
    goGroup :: (Set ICal.EventOccurrence, Set UnresolvedEventGroup) -> UnresolvedEventGroup -> (Set ICal.EventOccurrence, Set UnresolvedEventGroup)
    goGroup (allEvents, result) ueg =
      let (allEvents', groupEvents) = foldl' goEvent (allEvents, S.empty) $ S.toAscList $ unresolvedEvents ueg
          ueg' =
            UnresolvedEventGroup
              { unresolvedEventGroupStatic = unresolvedEventGroupStatic ueg,
                unresolvedEventGroupRecurrenceId = unresolvedEventGroupRecurrenceId ueg,
                unresolvedEvents = groupEvents
              }
          result' = S.insert ueg' result
       in (allEvents', result')
    goEvent :: (Set ICal.EventOccurrence, Set ICal.EventOccurrence) -> ICal.EventOccurrence -> (Set ICal.EventOccurrence, Set ICal.EventOccurrence)
    goEvent (allEvents, groupEvents) e = if S.member e allEvents then (allEvents, groupEvents) else (S.insert e allEvents, S.insert e groupEvents)
