{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- This module uses list as a monad a lot, make sure you understand it before reading this module.
module Smos.Calendar.Import.Recur where

import Control.Monad.Reader
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
        unresolvedEvents <- concat <$> mapM recurEvent recurringEvents
        let unresolvedEventsTimeZones = recurringEventsTimeZones
        pure UnresolvedEvents {..}

data RecurCtx = RecurCtx {recurCtxLimit :: LocalTime}
  deriving (Show, Eq)

type R = Reader RecurCtx

recurEvent :: RecurringEvent -> R [UnresolvedEvent]
recurEvent RecurringEvent {..} = do
  let unresolvedEventStatic = recurringEventStatic
  if S.null recurringEventRRules
    then do
      let unresolvedEventStart = recurringEventStart
      let unresolvedEventEnd = recurringEventEnd
      pure [UnresolvedEvent {..}]
    else fmap concat $ forM (S.toList recurringEventRRules) $ \rrule -> do
      tups <- recurMUnresolvedTimestamps rrule recurringEventStart recurringEventEnd
      pure $ do
        (unresolvedEventStart, unresolvedEventEnd) <- tups
        pure UnresolvedEvent {..}

recurMUnresolvedTimestamps :: RRule -> Maybe CalTimestamp -> Maybe CalEndDuration -> R [(Maybe CalTimestamp, Maybe CalEndDuration)]
recurMUnresolvedTimestamps rrule mstart mend = case (mstart, mend) of
  (Nothing, Nothing) -> pure [(Nothing, Nothing)] -- One occurrence, just to make sure we don't miss any events even if they're weird...
  (Just start, Nothing) -> do
    starts <- recurCalTimestamp rrule start
    pure $ (,) <$> (Just <$> starts) <*> pure Nothing
  (Nothing, Just end) -> do
    ends <- recurCalEndDuration rrule end
    pure $ (,) Nothing <$> (Just <$> ends)
  (Just start, Just end) -> do
    tups <- recurUnresolvedTimestamps rrule start end
    pure $ map (\(a, b) -> (Just a, Just b)) tups

recurUnresolvedTimestamps :: RRule -> CalTimestamp -> CalEndDuration -> R [(CalTimestamp, CalEndDuration)]
recurUnresolvedTimestamps rrule start end = do
  starts <- recurCalTimestamp rrule start
  pure $ expandEnds starts end
  where
    expandEnds :: [CalTimestamp] -> CalEndDuration -> [(CalTimestamp, CalEndDuration)]
    expandEnds starts end = case starts of
      [] -> []
      [start] -> [(start, end)]
      (start1 : start2 : rest) ->
        let diff = diffCalTimestamp start2 start1
            end2 = addEndDuration diff end -- This may be wrong when the end is in a different timezone than the start.
         in (start1, end) : expandEnds (start2 : rest) end2
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

recurCalEndDuration :: RRule -> CalEndDuration -> R [CalEndDuration]
recurCalEndDuration rrule = \case
  CalTimestamp cts -> fmap CalTimestamp <$> recurCalTimestamp rrule cts
  CalDuration i -> pure [CalDuration i]

recurCalTimestamp :: RRule -> CalTimestamp -> R [CalTimestamp]
recurCalTimestamp rrule = \case
  CalDateTime cdt -> fmap CalDateTime <$> recurCalDateTime rrule cdt
  CalDate d -> undefined

recurCalDateTime :: RRule -> CalDateTime -> R [CalDateTime]
recurCalDateTime rrule = \case
  Floating lt -> fmap Floating <$> recurLocalTime rrule lt
  UTC utct -> fmap UTC <$> recurUTCTime rrule utct
  Zoned lt tzid -> do
    lts <- recurLocalTime rrule lt
    pure $ Zoned <$> lts <*> pure tzid

recurUTCTime :: RRule -> UTCTime -> R [UTCTime]
recurUTCTime rrule utct = do
  let lt = utcToLocalTime utc utct
  lts <- recurLocalTime rrule lt
  pure $ localTimeToUTC utc <$> lts

recurLocalTime :: RRule -> LocalTime -> R [LocalTime]
recurLocalTime rrule lt = do
  limit <- asks recurCtxLimit
  pure $ S.toList $ rruleOccurrencesUntil lt rrule limit
