{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Pick (pickEvents) where

import Control.Monad
import qualified Data.ByteString.Lazy as LB
import Data.Default
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.Lazy as LT
import Data.Time
import qualified ICal
import qualified ICal.Parameter as ICal
import qualified ICal.Recurrence as ICal
import Smos.Calendar.Import.RecurringEvent
import Smos.Calendar.Import.Static

pickEvents :: Bool -> ICal.ICalendar -> Set RecurringEvents
pickEvents debug = S.fromList . map (pickEventsFromCalendar debug)

pickEventsFromCalendar :: Bool -> ICal.Calendar -> RecurringEvents
pickEventsFromCalendar debug ICal.Calendar {..} =
  let recurringEvents = pickEventMap debug calendarEvents
      recurringEventsTimeZones = pickTimeZoneMap calendarTimeZones
   in RecurringEvents {..}

pickEventMap :: Bool -> [ICal.Event] -> Map ICal.UID (Set RecurringEvent)
pickEventMap debug =
  M.fromListWith S.union
    . map
      ( \e ->
          ( ICal.eventUID e,
            S.singleton $
              let recurringEventEvent = ICal.getRecurringEvent e
                  staticSummary = ICal.unSummary <$> ICal.eventSummary e
                  staticDescription = case ICal.unDescription <$> ICal.eventDescription e of
                    Nothing -> Nothing
                    Just "" -> Nothing -- Don't pick the empty string, it's pointless.
                    Just d -> Just d
                  staticUID =
                    if debug
                      then Just $ ICal.unUID $ ICal.eventUID e
                      else Nothing
                  staticOriginalEvent =
                    if debug
                      then Just $ ICal.renderComponentText e
                      else Nothing
                  recurringEventStatic = Static {..}
               in RecurringEvent {..}
          )
      )

pickTimeZoneMap :: [ICal.TimeZone] -> Map ICal.TZIDParam ICal.TimeZone
pickTimeZoneMap = M.fromList . map (\tz -> (ICal.tzidParam $ ICal.timeZoneId tz, tz))
