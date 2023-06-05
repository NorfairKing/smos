{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Pick
  ( pickTimeZones,
    pickEvents,
  )
where

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

pickTimeZones :: ICal.Calendar -> Map ICal.TimeZoneIdentifierParam ICal.TimeZone
pickTimeZones = ICal.calendarTimeZoneMap

pickEvents :: Bool -> ICal.Calendar -> RecurringEvents
pickEvents debug = RecurringEvents . pickEventMap debug . ICal.calendarEvents

pickEventMap :: Bool -> [ICal.Event] -> Map ICal.UID (Set RecurringEvent)
pickEventMap debug =
  M.fromListWith S.union
    . mapMaybe
      ( \e -> do
          -- Don't pick cancelled events
          guard $ ICal.eventStatus e /= Just ICal.StatusCancelled
          let mDescription = case ICal.descriptionContents <$> ICal.eventDescription e of
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
                    staticSummary = ICal.summaryContents <$> ICal.eventSummary e
                    staticDescription = mDescription
                    staticBusy =
                      case ICal.eventTransparency e of
                        ICal.TransparencyTransparent -> False
                        ICal.TransparencyOpaque -> True
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
