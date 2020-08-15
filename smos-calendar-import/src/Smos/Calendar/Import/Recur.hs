{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- This module uses list as a monad a lot, make sure you understand it before reading this module.
module Smos.Calendar.Import.Recur where

import Control.Monad.Reader
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time
import Smos.Calendar.Import.RecurrenceRule
import Smos.Calendar.Import.RecurringEvent
import Smos.Calendar.Import.UnresolvedEvent
import Smos.Calendar.Import.UnresolvedTimestamp

recurEvents :: LocalTime -> [RecurringEvents] -> [UnresolvedEvents]
recurEvents limit res = map (recurRecurringEvents limit) res

recurRecurringEvents :: LocalTime -> RecurringEvents -> UnresolvedEvents
recurRecurringEvents limit RecurringEvents {..} =
  let ctx = RecurCtx {recurCtxLimit = limit}
   in flip runReader ctx $ do
        unresolvedEventGroups <- mapM recurEvent recurringEvents
        let unresolvedEventsTimeZones = recurringEventsTimeZones
        pure UnresolvedEvents {..}

data RecurCtx = RecurCtx {recurCtxLimit :: LocalTime}
  deriving (Show, Eq)

type R = Reader RecurCtx

recurEvent :: RecurringEvent -> R UnresolvedEventGroup
recurEvent RecurringEvent {..} = do
  let unresolvedEventGroupStatic = recurringEventStatic
  if recurringEventRecurrence == emptyRecurrence
    then do
      let unresolvedEventStart = recurringEventStart
      let unresolvedEventEnd = recurringEventEnd
      let unresolvedEvents = [UnresolvedEvent {..}]
      pure UnresolvedEventGroup {..}
    else do
      unresolvedEvents <- do
        tups <- recurMUnresolvedTimestamps recurringEventRecurrence recurringEventStart recurringEventEnd
        pure $ do
          (unresolvedEventStart, unresolvedEventEnd) <- tups
          pure UnresolvedEvent {..}
      pure UnresolvedEventGroup {..}

recurMUnresolvedTimestamps :: Recurrence -> Maybe CalTimestamp -> Maybe CalEndDuration -> R [(Maybe CalTimestamp, Maybe CalEndDuration)]
recurMUnresolvedTimestamps recurrence mstart mend = case (mstart, mend) of
  (Nothing, Nothing) -> pure [(Nothing, Nothing)] -- One occurrence, just to make sure we don't miss any events even if they're weird...
  (Just start, Nothing) -> do
    starts <- S.toAscList <$> recurCalTimestamp recurrence start
    pure $ (,) <$> (Just <$> starts) <*> pure Nothing
  (Nothing, Just end) -> do
    ends <- S.toAscList <$> recurCalEndDuration recurrence end
    pure $ (,) Nothing <$> (Just <$> ends)
  (Just start, Just end) -> do
    tups <- recurUnresolvedTimestamps recurrence start end
    pure $ map (\(a, b) -> (Just a, Just b)) tups

recurUnresolvedTimestamps :: Recurrence -> CalTimestamp -> CalEndDuration -> R [(CalTimestamp, CalEndDuration)]
recurUnresolvedTimestamps recurrence start end = do
  starts <- S.toAscList <$> recurCalTimestamp recurrence start
  pure $ expandEnds starts end
  where
    expandEnds :: [CalTimestamp] -> CalEndDuration -> [(CalTimestamp, CalEndDuration)]
    expandEnds starts end1 = case starts of
      [] -> []
      [start1] -> [(start1, end1)]
      (start1 : start2 : rest) ->
        let diff = diffCalTimestamp start2 start1
            end2 = addEndDuration diff end1 -- This may be wrong when the end is in a different timezone than the start.
         in (start1, end1) : expandEnds (start2 : rest) end2
      where
        -- This is not total, but it's an internal function and the function maintains the invariant that the timestamps are similar.
        diffCalTimestamp :: CalTimestamp -> CalTimestamp -> NominalDiffTime
        diffCalTimestamp cts1 cts2 = case (cts1, cts2) of
          (CalDateTime cdt1, CalDateTime cdt2) -> diffCalDateTime cdt1 cdt2
          (CalDate cd1, CalDate cd2) -> fromInteger (diffDays cd1 cd2) * nominalDay
          _ -> error "diffCalTimestamp: Should not happen"
        diffCalDateTime :: CalDateTime -> CalDateTime -> NominalDiffTime
        diffCalDateTime cdt1 cdt2 = case (cdt1, cdt2) of
          (Floating lt1, Floating lt2) -> diffLocalTime lt1 lt2
          (UTC utct1, UTC utct2) -> diffUTCTime utct1 utct2
          (Zoned lt1 _, Zoned lt2 _) -> diffLocalTime lt1 lt2
          _ -> error "diffCalDateTime: Should not happen"
        addEndDuration :: NominalDiffTime -> CalEndDuration -> CalEndDuration
        addEndDuration ndt = \case
          CalDuration i -> CalDuration i
          CalTimestamp ts -> CalTimestamp $ addTimestamp ndt ts
        addTimestamp :: NominalDiffTime -> CalTimestamp -> CalTimestamp
        addTimestamp ndt = \case
          CalDateTime cdt -> CalDateTime $ addDateTime ndt cdt
          CalDate d -> CalDate $ addDate ndt d
        -- This could go wrong if the nominal diff time is not a multiple of a day, but that should not happen with valid recurrence rules.
        addDate :: NominalDiffTime -> Day -> Day
        addDate ndt d =
          let lt = LocalTime d midnight
           in localDay $ addLocalTime ndt lt
        addDateTime :: NominalDiffTime -> CalDateTime -> CalDateTime
        addDateTime ndt = \case
          Floating lt -> Floating $ addLocalTime ndt lt
          UTC utct -> UTC $ addUTCTime ndt utct
          Zoned lt tzid -> Zoned (addLocalTime ndt lt) tzid

recurCalEndDuration :: Recurrence -> CalEndDuration -> R (Set CalEndDuration)
recurCalEndDuration recurrence = \case
  CalTimestamp cts -> S.map CalTimestamp <$> recurCalTimestamp recurrence cts
  CalDuration i -> pure $ S.singleton $ CalDuration i

recurCalTimestamp :: Recurrence -> CalTimestamp -> R (Set CalTimestamp)
recurCalTimestamp Recurrence {..} cts = do
  rRuleSet <- case cts of
    CalDateTime cdt -> S.map CalDateTime <$> recurCalDateTime recurrenceRules cdt
    CalDate dt -> S.map CalDate <$> recurDate recurrenceRules dt
  let posSet = rRuleSet `S.union` recurrenceRDates
  pure $ posSet `S.difference` recurrenceExceptions

recurCalDateTime :: Set RRule -> CalDateTime -> R (Set CalDateTime)
recurCalDateTime rules = \case
  Floating lt -> S.map Floating <$> recurLocalTime rules lt
  UTC utct -> S.map UTC <$> recurUTCTime rules utct
  Zoned lt tzid -> do
    lts <- recurLocalTime rules lt
    pure $ S.map (`Zoned` tzid) lts

recurUTCTime :: Set RRule -> UTCTime -> R (Set UTCTime)
recurUTCTime rules utct = do
  let lt = utcToLocalTime utc utct
  lts <- recurLocalTime rules lt
  pure $ S.map (localTimeToUTC utc) lts

recurLocalTime :: Set RRule -> LocalTime -> R (Set LocalTime)
recurLocalTime rules lt = do
  limit <- asks recurCtxLimit
  pure $ S.unions
    $ flip map (S.toList rules)
    $ \rrule -> rruleDateTimeOccurrencesUntil lt rrule limit

recurDate :: Set RRule -> Day -> R (Set Day)
recurDate rules d = do
  limit <- asks recurCtxLimit
  pure $ S.unions
    $ flip map (S.toList rules)
    $ \rrule -> rruleDateOccurrencesUntil d rrule $ localDay limit
