{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Pick
  ( pickTimeZones,
    pickEvents,
  )
where

import Data.Map (Map)
import qualified Data.Map as M
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
    . map
      ( \e ->
          ( ICal.eventUID e,
            S.singleton (pickedRecurringEvent debug e)
          )
      )

-- | What smos keeps of one component
--
-- Every component is picked, including the ones that will not be imported.
-- Whether to import is recorded rather than acted on, because a component with
-- a RECURRENCE-ID has to reach recurrence to replace the instance it names; see
-- 'Inclusion'.
pickedRecurringEvent :: Bool -> ICal.Event -> RecurringEvent
pickedRecurringEvent debug e =
  let recurring = ICal.eventRecurring e
   in RecurringEvent
        { recurringEventStatic = pickedStatic debug e,
          recurringEventSequenceNumber = ICal.recurringSequenceNumber recurring,
          recurringEventRecurrenceIdentifier = ICal.recurringRecurrenceIdentifier recurring,
          recurringEventStart = ICal.recurringStart recurring,
          recurringEventEndOrDuration = ICal.recurringEnd recurring,
          recurringEventRecurrence = ICal.recurringRecurrence recurring
        }

pickedStatic :: Bool -> ICal.Event -> Static
pickedStatic debug e =
  let staticSummary = ICal.summaryContents <$> ICal.eventSummary e
      staticDescription = case ICal.descriptionContents <$> ICal.eventDescription e of
        Nothing -> Nothing
        Just "" -> Nothing -- Don't pick the empty string, it's pointless.
        Just d -> Just d
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
      staticInclusion =
        if ICal.eventStatus e == Just ICal.StatusCancelled
          || maybe False ("SMOS_NO_CALENDAR_IMPORT" `T.isInfixOf`) staticDescription
          then Exclude
          else Include
   in Static {..}
