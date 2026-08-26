{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Recur (recurRecurringEvents) where

import Data.Functor (void)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time
import qualified ICal.Recurrence as ICal
import Smos.Calendar.Import.RecurringEvent
import Smos.Calendar.Import.UnresolvedEvent

-- | Recur every picked component, and gather the instances by the component
-- they came from
--
-- Recurrence happens over each UID's whole group of components at once, because
-- a component with a RECURRENCE-ID replaces an instance of the series rather
-- than adding one beside it.  So an instance's properties are the overriding
-- component's where it has one and the series' otherwise, which is why the rest
-- of the pipeline is grouped by contributing component rather than by UID.
recurRecurringEvents :: Day -> RecurringEvents -> ICal.R UnresolvedEvents
recurRecurringEvents limit recurringEvents = do
  recurrence <- ICal.recur limit (recurringOf recurringEvents)
  let byStatic =
        M.fromListWith
          S.union
          [ (static, S.singleton (void occurrence))
          | occurrences <- M.elems recurrence,
            occurrence <- S.toList occurrences,
            let (inclusion, static) = ICal.occurrenceComponent occurrence,
            inclusion == Include
          ]
  let unresolvedEventGroups =
        S.fromList $
          map
            ( \(unresolvedEventGroupStatic, unresolvedEvents) ->
                UnresolvedEventGroup {..}
            )
            (M.toList byStatic)
  pure UnresolvedEvents {..}
