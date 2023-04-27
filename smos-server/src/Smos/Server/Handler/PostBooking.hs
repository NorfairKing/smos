{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.PostBooking
  ( servePostBooking,
    makeICALCalendar,
  )
where

import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time.Zones
import Data.Time.Zones.All
import ICal
import Network.URI
import Smos.Server.Handler.GetBookingSlots
import Smos.Server.Handler.Import

servePostBooking :: Username -> Booking -> ServerHandler ICalendar
servePostBooking username booking = withUsernameId username $ \uid -> do
  mBookingConfig <- runDB $ getBy $ UniqueBookingConfigUser uid
  case mBookingConfig of
    Nothing -> throwError err404
    Just (Entity _ bookingConfig) -> do
      BookingSlots {..} <- computeBookingSlots uid
      let localTime = utcToLocalTimeTZ (tzByLabel (bookingConfigTimeZone bookingConfig)) (bookingUTCTime booking)
      if localTime `M.notMember` bookingSlots
        then throwError err400
        else do
          now <- liftIO getCurrentTime
          uuid <- nextRandomUUID
          let cal = makeICALCalendar now uuid bookingConfig booking
          pure [cal]

makeICALCalendar ::
  UTCTime ->
  UUID ICal.Event ->
  BookingConfig ->
  Booking ->
  ICal.Calendar
makeICALCalendar now uuid bc b =
  (makeCalendar (ICal.ProdId "-//CS SYD//Smos//EN"))
    { calendarMethod = Just (ICal.Method "REQUEST"),
      calendarEvents =
        [ makeICALEvent now uuid bc b
        ]
    }

makeICALEvent ::
  UTCTime ->
  UUID ICal.Event ->
  BookingConfig ->
  Booking ->
  ICal.Event
makeICALEvent now uuid BookingConfig {..} Booking {..} =
  let mUserURI = parseURIReference $ "mailto:" <> T.unpack bookingConfigEmailAddress
      mUserCalAddress = CalAddress <$> mUserURI

      userCommonName = CommonName (QuotedParam bookingConfigName)
      mClientURI = parseURIReference $ "mailto:" <> T.unpack bookingClientEmailAddress
      mClientCalAddress = CalAddress <$> mClientURI
      clientCommonName = CommonName (QuotedParam bookingClientName)

      simplifyName = concatMap $ \case
        ' ' -> []
        c -> [c]

      mJitsiLink =
        parseURIReference $
          "https://meet.jit.si/"
            <> escapeURIString
              isUnescapedInURIComponent
              ( concat
                  [ simplifyName (T.unpack bookingClientName),
                    "Meets",
                    simplifyName (T.unpack bookingConfigName)
                  ]
              )
   in ( makeEvent
          (ICal.UID $ uuidText uuid)
          (DateTimeStamp (DateTimeUTC now))
      )
        { eventDateTimeStart = Just $ DateTimeStartDateTime $ DateTimeUTC bookingUTCTime,
          eventDateTimeEndDuration = Just (Right (nominalDiffTimeDuration bookingDuration)),
          eventClassification = ClassificationPrivate,
          eventCreated = Just (Created now),
          -- TODO: Nice event description
          -- TODO: jitsi link in description, url, and location
          eventDescription = Just (Description "Nice description"),
          eventLocation = Just (Location "Nice location"),
          eventOrganizer =
            ( \clientCalAddress ->
                ( (mkOrganizer clientCalAddress)
                    { organizerCommonName = Just clientCommonName
                    }
                )
            )
              <$> mClientCalAddress,
          eventURL = URL . ICal.URI <$> mJitsiLink,
          eventStatus = Just StatusTentative,
          -- TODO: Nice event summary
          eventSummary = Just (Summary "Nice Summary"),
          eventTransparency = TransparencyOpaque,
          eventAttendees =
            S.fromList $
              catMaybes
                [ do
                    userCalAddress <- mUserCalAddress
                    pure $
                      (mkAttendee userCalAddress)
                        { attendeeParticipationRole = ParticipationRoleRequiredParticipant,
                          attendeeParticipationStatus = ParticipationStatusTentative,
                          attendeeRSVPExpectation = RSVPExpectationTrue,
                          attendeeCommonName = Just userCommonName
                        },
                  do
                    clientCalAddress <- mClientCalAddress
                    pure $
                      (mkAttendee clientCalAddress)
                        { attendeeParticipationRole = ParticipationRoleRequiredParticipant,
                          attendeeParticipationStatus = ParticipationStatusAccepted,
                          attendeeRSVPExpectation = RSVPExpectationFalse,
                          attendeeCommonName = Just clientCommonName
                        }
                ]
        }
