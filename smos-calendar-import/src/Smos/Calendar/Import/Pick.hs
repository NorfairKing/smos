{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Pick where

import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Validity
import GHC.Generics
import Smos.Calendar.Import.RecurringEvent
import Text.ICalendar.Types

pickEvents :: [VCalendar] -> [RecurringEvent]
pickEvents = concatMap pickEventsFromCalendar

pickEventsFromCalendar :: VCalendar -> [RecurringEvent]
pickEventsFromCalendar VCalendar {..} = map pickEventFromVEvent $ M.elems vcEvents

pickEventFromVEvent :: VEvent -> RecurringEvent
pickEventFromVEvent VEvent {..} =
  let recurringEventSummary = LT.toStrict . summaryValue <$> veSummary
      recurringEventDescription = LT.toStrict . descriptionValue <$> veDescription
   in RecurringEvent {..}
