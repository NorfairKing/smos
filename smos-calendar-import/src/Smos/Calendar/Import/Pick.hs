{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Pick (pickEvents) where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified ICal
import qualified ICal.Recurrence as ICal
import Smos.Calendar.Import.RecurringEvent
import Smos.Calendar.Import.Static

pickEvents :: Bool -> ICal.ICalendar -> Set RecurringEvents
pickEvents debug = S.fromList . map (pickEventsFromCalendar debug)

pickEventsFromCalendar :: Bool -> ICal.Calendar -> RecurringEvents
pickEventsFromCalendar debug cal@ICal.Calendar {..} =
  let recurringEvents = pickEventMap debug calendarEvents
      recurringEventsTimeZones = ICal.calendarTimeZoneMap cal
   in RecurringEvents {..}

pickEventMap :: Bool -> [ICal.Event] -> Map ICal.UID (Set RecurringEvent)
pickEventMap debug =
  M.fromListWith S.union
    . mapMaybe
      ( \e -> do
          -- Don't pick cancelled events
          guard $ ICal.eventStatus e /= Just ICal.StatusCancelled
          let mDescription = case ICal.unDescription <$> ICal.eventDescription e of
                Nothing -> Nothing
                Just "" -> Nothing -- Don't pick the empty string, it's pointless.
                Just d -> Just d
          -- Don't pick events with "SMOS_NO_CALENDAR_IMPORT" in the description
          guard $ case mDescription of
            Nothing -> True
            Just d -> not $ "SMOS_NO_CALENDAR_IMPORT" `T.isInfixOf` d
          pure
            ( ICal.eventUID e,
              S.singleton $
                let recurringEventEvent = ICal.getRecurringEvent e
                    staticSummary = ICal.unSummary <$> ICal.eventSummary e
                    staticDescription = mDescription
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
