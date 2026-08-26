{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Calendar.Import.Pick
  ( pickTimeZones,
    pickEvents,
    pickedRecurringEvent,
    pickedStatic,
    pickedInclusion,
    pickedBusy,
  )
where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
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
-- Every component is picked, including the ones that will not be imported:
-- whether to import is recorded rather than acted on, because a component with
-- a RECURRENCE-ID has to reach recurrence to replace the instance it names.
pickedRecurringEvent :: Bool -> ICal.Event -> RecurringEvent
pickedRecurringEvent debug e =
  let recurring = ICal.eventRecurring e
   in RecurringEvent
        { recurringEventStatic = pickedStatic debug e,
          recurringEventInclusion = pickedInclusion e,
          recurringEventSequenceNumber = ICal.recurringSequenceNumber recurring,
          recurringEventRecurrenceIdentifier = ICal.recurringRecurrenceIdentifier recurring,
          recurringEventStart = ICal.recurringStart recurring,
          recurringEventEndOrDuration = ICal.recurringEnd recurring,
          recurringEventRecurrence = ICal.recurringRecurrence recurring
        }

pickedStatic :: Bool -> ICal.Event -> Static
pickedStatic debug e =
  let staticSummary = ICal.summaryContents <$> ICal.eventSummary e
      staticDescription = pickedDescription e
      staticBusy = pickedBusy e
      staticUID =
        if debug
          then Just $ ICal.unUID $ ICal.eventUID e
          else Nothing
      staticOriginalEvent =
        if debug
          then Just $ ICal.renderComponentText e
          else Nothing
   in Static {..}

-- | Whether this event blocks out the time it occupies
--
-- Both properties bear on it: TRANSP says whether the event would consume
-- time, and STATUS says whether it is happening at all.  A declined ATTENDEE
-- would too, but there is no setting saying which attendee is us, and taking
-- any attendee's refusal would make a twenty-person meeting free because one
-- of them could not come.
pickedBusy :: ICal.Event -> Busyness
pickedBusy e = case ICal.eventTransparency e of
  ICal.TransparencyTransparent -> Free
  ICal.TransparencyOpaque -> case ICal.eventStatus e of
    Just ICal.StatusTentative -> Free
    Just ICal.StatusConfirmed -> Busy
    Just ICal.StatusCancelled -> Busy
    -- Section 3.8.1.11 gives these to a VTODO or a VJOURNAL, so a VEVENT
    -- carrying one is not conforming and says nothing about its time.  They
    -- are enumerated rather than swept into a catch-all so that a new status
    -- has to be considered here.
    Just ICal.StatusNeedsAction -> Busy
    Just ICal.StatusInProgress -> Busy
    Just ICal.StatusDraft -> Busy
    Just ICal.StatusFinal -> Busy
    Nothing -> Busy

pickedDescription :: ICal.Event -> Maybe Text
pickedDescription e = case ICal.descriptionContents <$> ICal.eventDescription e of
  Nothing -> Nothing
  Just "" -> Nothing -- Don't pick the empty string, it's pointless.
  Just d -> Just d

-- | Whether this component's instances belong in the imported calendar
--
-- Neither of these can be acted on by dropping the component here; see
-- 'Inclusion' for why.
pickedInclusion :: ICal.Event -> Inclusion
pickedInclusion e
  | ICal.eventStatus e == Just ICal.StatusCancelled = Exclude
  | maybe False ("SMOS_NO_CALENDAR_IMPORT" `T.isInfixOf`) (pickedDescription e) = Exclude
  | otherwise = Include
