{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Pick where

import Control.Monad.Reader
import qualified Data.Map as M
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Data.Time
import Smos.Calendar.Import.RecurringEvent
import Text.ICalendar.Types as ICal

pickEvents :: [ICal.VCalendar] -> [RecurringEvent]
pickEvents = concatMap pickEventsFromCalendar

pickEventsFromCalendar :: ICal.VCalendar -> [RecurringEvent]
pickEventsFromCalendar ICal.VCalendar {..} =
  let ctx =
        PickCtx
          { pickCtxTimeZones = M.mapKeys LT.toStrict vcTimeZones
          }
   in flip runReader ctx $ mapM pickEventFromVEvent $ M.elems vcEvents

type P = Reader PickCtx

data PickCtx
  = PickCtx
      { pickCtxTimeZones :: Map Text VTimeZone
      }
  deriving (Show, Eq)

pickEventFromVEvent :: ICal.VEvent -> P RecurringEvent
pickEventFromVEvent ICal.VEvent {..} = do
  let recurringEventSummary = LT.toStrict . ICal.summaryValue <$> veSummary
      recurringEventDescription = LT.toStrict . ICal.descriptionValue <$> veDescription
  recurringEventStart <- mapM pickStart veDTStart
  recurringEventEnd <- mapM pickEndDuration veDTEndDuration
  pure $ RecurringEvent {..}

pickStart :: ICal.DTStart -> P CalTimestamp
pickStart = \case
  DTStartDateTime dt _ -> CalDateTime <$> pickDateTime dt
  DTStartDate d _ -> pure $ CalDate $ pickDate d

pickEndDuration :: Either ICal.DTEnd ICal.DurationProp -> P CalEndDuration
pickEndDuration = \case
  Left e -> CalTimestamp <$> pickEnd e
  Right d -> pure $ CalDuration $ pickDurationProp d

pickEnd :: ICal.DTEnd -> P CalTimestamp
pickEnd = \case
  DTEndDateTime dt _ -> CalDateTime <$> pickDateTime dt
  DTEndDate d _ -> pure $ CalDate $ pickDate d

pickDateTime :: ICal.DateTime -> P CalDateTime
pickDateTime = \case
  FloatingDateTime lt -> pure $ Floating lt
  UTCDateTime utct -> pure $ UTC utct
  ZonedDateTime lt tzid -> do
    mtz <- asks (M.lookup (LT.toStrict tzid) . pickCtxTimeZones)
    pure $ case mtz of
      Nothing -> Floating lt
      Just vtz -> Floating $ pickZonedTime vtz lt -- FIXME

pickZonedTime :: VTimeZone -> LocalTime -> LocalTime
pickZonedTime VTimeZone {..} lt = lt

pickDate :: ICal.Date -> Day
pickDate (Date d) = d

pickDurationProp :: ICal.DurationProp -> Int
pickDurationProp (DurationProp d _) = pickDuration d

pickDuration :: ICal.Duration -> Int
pickDuration = \case
  DurationDate sign d h m s -> signNum sign * (((d * 24 + h) * 60 + m) * 60 + s)
  DurationTime sign h m s -> signNum sign * ((h * 60 + m) * 60 + s)
  DurationWeek sign w -> signNum sign * w * 7 * 24 * 60 * 60

signNum :: Num a => ICal.Sign -> a
signNum Positive = 1
signNum Negative = -1
