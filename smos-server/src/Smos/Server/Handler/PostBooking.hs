{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-pattern-binds #-}

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

      summary =
        T.pack $
          unwords
            [ T.unpack bookingClientName,
              "<>",
              T.unpack bookingConfigName
            ]

      description =
        let BookingConfig _ _ _ _ = undefined
            Booking _ _ _ _ _ = undefined
         in T.pack $
              unlines $
                concat
                  [ [ unwords
                        [ T.unpack bookingClientName,
                          "meets",
                          T.unpack bookingConfigName
                        ],
                      "",
                      unwords
                        [ "For",
                          T.unpack bookingConfigName <> ":"
                        ],
                      formatTime defaultTimeLocale "%A %F %H:%M" $ utcToLocalTimeTZ (tzByLabel bookingConfigTimeZone) bookingUTCTime,
                      "",
                      unwords
                        [ "For",
                          T.unpack bookingClientName <> ":"
                        ],
                      formatTime defaultTimeLocale "%A %F %H:%M" $ utcToLocalTimeTZ (tzByLabel bookingClientTimeZone) bookingUTCTime
                    ],
                    [ unwords ["Meeting link:", show uri]
                      | uri <- maybeToList mJitsiLink
                    ]
                  ]
   in ( makeEvent
          (ICal.UID $ uuidText uuid)
          (DateTimeStamp (DateTimeUTC now))
      )
        { -- "Just" using UTCTime here is valid because there is no recurrence and we use a timezone database.
          eventDateTimeStart = Just $ DateTimeStartDateTime $ DateTimeUTC bookingUTCTime,
          eventDateTimeEndDuration = Just (Right (nominalDiffTimeDuration bookingDuration)),
          eventClassification = ClassificationPrivate,
          eventCreated = Just (Created now),
          eventDescription = Just $ Description description,
          eventLocation = Location . T.pack . show <$> mJitsiLink,
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
          eventSummary = Just $ Summary summary,
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
