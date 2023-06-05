{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-pattern-binds #-}

module Smos.Server.Handler.PostBooking
  ( servePostBooking,
    makeBookingEmail,
    makeEmailSubject,
    makeEmailHtml,
    makeEmailText,
    makeICALCalendar,
  )
where

import qualified Amazonka as AWS
import qualified Amazonka.SES as SES
import qualified Amazonka.SES.SendRawEmail as SES
import Control.Monad.Logger
import Control.Retry
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Time.Zones
import Data.Time.Zones.All
import ICal
import Network.HTTP.Client as HTTP
import Network.HTTP.Types as HTTP
import Network.Mail.Mime
import Network.URI
import Smos.Data
import Smos.Server.Booking (getUserBookingSettings)
import Smos.Server.Handler.GetBookingSlots
import Smos.Server.Handler.Import
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet
import Text.Shakespeare.Text
import UnliftIO

servePostBooking :: Username -> Booking -> ServerHandler ICalendar
servePostBooking username booking = withUsernameId username $ \uid -> do
  mBookingSettings <- getUserBookingSettings uid
  case mBookingSettings of
    Nothing -> throwError err404
    Just bookingSettings -> do
      if bookingDuration booking `S.member` bookingSettingAllowedDurations bookingSettings
        then do
          BookingSlots {..} <- computeBookingSlots uid (bookingDuration booking) bookingSettings
          let localTime = utcToLocalTimeTZ (tzByLabel (bookingSettingTimeZone bookingSettings)) (bookingUTCTime booking)
          if localTime `M.notMember` bookingSlots
            then throwError err400
            else do
              now <- liftIO getCurrentTime
              uuid <- nextRandomUUID
              let ical = [makeICALCalendar now uuid bookingSettings booking]
              sendBookingEmail bookingSettings booking ical
              pure ical
        else throwError $ err400 {errBody = "This duration is not allowed."}

sendBookingEmail :: BookingSettings -> Booking -> ICal.ICalendar -> ServerHandler ()
sendBookingEmail bookingSettings@BookingSettings {..} booking@Booking {..} ical = do
  mSendAddress <- asks serverEnvBookingEmailAddress
  case mSendAddress of
    Nothing ->
      logWarnN $
        T.pack $
          unwords
            [ "Not sending booking email because no send address has been configured from",
              show bookingClientEmailAddress,
              "to",
              show bookingSettingEmailAddress
            ]
    Just sendEmailAddress -> do
      logInfoN $
        T.pack $
          unwords
            [ "Sending booking email from",
              show bookingClientEmailAddress,
              "to",
              show bookingSettingEmailAddress
            ]

      let mail = makeBookingEmail sendEmailAddress bookingSettings booking ical
      rawEmailBs <- liftIO $ LB.toStrict <$> renderMail' mail

      let rawMessage = SES.newRawMessage rawEmailBs
      let request = SES.newSendRawEmail rawMessage

      logFunc <- askLoggerIO
      errOrResponse <- liftIO $ runLoggingT (runAWS request) logFunc

      case SES.httpStatus <$> errOrResponse of
        Right 200 -> pure ()
        _ -> throwError $ err500 {errBody = "Failed to send email."}

makeBookingEmail :: Text -> BookingSettings -> Booking -> ICalendar -> Mail
makeBookingEmail sendEmailAddress bookingSetting@BookingSettings {..} booking@Booking {..} ical =
  let sendAddress = Address {addressName = Nothing, addressEmail = sendEmailAddress}
      userAddress =
        Address
          { addressName = Just bookingSettingName,
            addressEmail = bookingSettingEmailAddress
          }
      clientAddress =
        Address
          { addressName = Just bookingClientName,
            addressEmail = bookingClientEmailAddress
          }
      subject = makeEmailSubject bookingSetting booking
      htmlContent = makeEmailHtml bookingSetting booking
      textContent = makeEmailText bookingSetting booking
      icalBS = renderICalendarByteString ical
   in addAttachmentBS "text/calendar" "invitation.ics" (LB.fromStrict icalBS)
        $ addPart
          [ htmlPart $ LT.fromStrict htmlContent,
            plainPart $ LT.fromStrict textContent
          ]
        $ (emptyMail sendAddress)
          { mailTo = [userAddress, clientAddress],
            mailHeaders =
              [ ("Subject", subject),
                ("Reply-To", renderAddress sendAddress)
              ]
          }

runAWS ::
  ( MonadUnliftIO m,
    MonadLoggerIO m,
    Typeable a,
    Typeable (AWS.AWSResponse a),
    AWS.AWSRequest a
  ) =>
  a ->
  m (Either AWS.Error (AWS.AWSResponse a))
runAWS request = do
  logger <- mkAwsLogger
  discoveredEnv <- liftIO $ AWS.newEnv AWS.discover
  let awsEnv =
        discoveredEnv
          { AWS.logger = logger,
            AWS.region = AWS.Ireland
          }

  let shouldRetry = \case
        Left awsError -> do
          logWarnN $ T.pack $ unlines ["Failed to contact AWS:", show awsError]
          case awsError of
            AWS.TransportError exception -> pure $ shouldRetryHttpException exception
            AWS.SerializeError _ -> pure False
            AWS.ServiceError (AWS.ServiceError' _ status _ _ _ _) -> pure $ shouldRetryStatusCode status
        Right _ -> pure False -- Didn't even fail.
  let tryOnce RetryStatus {..} = do
        unless (rsIterNumber == 0) $ logWarnN "Retrying AWS request"
        AWS.runResourceT $ AWS.sendEither awsEnv request

  retrying awsRetryPolicy (const shouldRetry) tryOnce

awsRetryPolicy :: RetryPolicy
awsRetryPolicy = exponentialBackoff 1_000_000 <> limitRetries 5

shouldRetryHttpException :: HttpException -> Bool
shouldRetryHttpException exception = case exception of
  InvalidUrlException _ _ -> False
  HttpExceptionRequest _ exceptionContent ->
    case exceptionContent of
      ResponseTimeout -> True
      ConnectionTimeout -> True
      ConnectionFailure _ -> True
      InternalException _ -> True
      ProxyConnectException _ _ _ -> True
      NoResponseDataReceived -> True
      ResponseBodyTooShort _ _ -> True
      InvalidChunkHeaders -> True
      IncompleteHeaders -> True
      HttpZlibException _ -> True
      ConnectionClosed -> True
      _ -> False

shouldRetryStatusCode :: HTTP.Status -> Bool
shouldRetryStatusCode status =
  let c = HTTP.statusCode status
   in c >= 500 && c < 600

mkAwsLogger :: MonadLoggerIO m => m AWS.Logger
mkAwsLogger = do
  logFunc <- askLoggerIO
  let logger awsLevel builder =
        let ourLevel = case awsLevel of
              AWS.Info -> LevelInfo
              AWS.Error -> LevelError
              AWS.Debug -> LevelDebug
              AWS.Trace -> LevelDebug
         in logFunc defaultLoc "aws-client" ourLevel $ toLogStr builder
  pure logger

makeEmailSubject :: BookingSettings -> Booking -> Text
makeEmailSubject BookingSettings {..} Booking {..} =
  T.pack $
    unwords
      [ "Smos Booking: Calendar invite for",
        T.unpack bookingSettingName,
        "from",
        T.unpack bookingClientName
      ]

makeEmailHtml :: BookingSettings -> Booking -> Text
makeEmailHtml BookingSettings {..} Booking {..} =
  let BookingSettings _ _ _ _ _ _ _ _ _ = undefined
      Booking _ _ _ _ _ _ = undefined
   in LT.toStrict $ renderHtml $(shamletFile "templates/booking.hamlet")

makeEmailText :: BookingSettings -> Booking -> Text
makeEmailText BookingSettings {..} Booking {..} =
  let BookingSettings _ _ _ _ _ _ _ _ _ = undefined
      Booking _ _ _ _ _ _ = undefined
   in LT.toStrict $(stextFile "templates/booking.txt")

makeICALCalendar ::
  UTCTime ->
  UUID ICal.Event ->
  BookingSettings ->
  Booking ->
  ICal.Calendar
makeICALCalendar now uuid bc b =
  (makeCalendar (ICal.ProductIdentifier "-//CS SYD//Smos//EN"))
    { calendarMethod = Just (ICal.Method "REQUEST"),
      calendarEvents =
        [ makeICALEvent now uuid bc b
        ]
    }

makeICALEvent ::
  UTCTime ->
  UUID ICal.Event ->
  BookingSettings ->
  Booking ->
  ICal.Event
makeICALEvent now uuid BookingSettings {..} Booking {..} =
  let mUserURI = parseURIReference $ "mailto:" <> T.unpack bookingSettingEmailAddress
      mUserCalAddress = CalAddress <$> mUserURI

      userCommonName = CommonName (QuotedParam bookingSettingName)
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
                    simplifyName (T.unpack bookingSettingName)
                  ]
              )

      summary =
        T.pack $
          unwords
            [ T.unpack bookingClientName,
              "<>",
              T.unpack bookingSettingName
            ]

      description =
        let BookingSettings _ _ _ _ _ _ _ _ _ = undefined
            Booking _ _ _ _ _ _ = undefined
         in T.pack $
              unlines $
                concat
                  [ [ unwords
                        [ T.unpack bookingClientName,
                          "meets",
                          T.unpack bookingSettingName
                        ],
                      "",
                      unwords
                        [ "For",
                          T.unpack bookingSettingName <> ":"
                        ],
                      formatTime defaultTimeLocale "%A %F %H:%M" $ utcToLocalTimeTZ (tzByLabel bookingSettingTimeZone) bookingUTCTime,
                      "",
                      unwords
                        [ "For",
                          T.unpack bookingClientName <> ":"
                        ],
                      formatTime defaultTimeLocale "%A %F %H:%M" $ utcToLocalTimeTZ (tzByLabel bookingClientTimeZone) bookingUTCTime
                    ],
                    [T.unpack extraInfo | extraInfo <- maybeToList bookingExtraInfo],
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
          eventDateTimeEndDuration = Just (Right (nominalDiffTimeDuration (fromIntegral bookingDuration * 60))),
          eventClassification = ClassificationPrivate,
          eventCreated = Just (Created now),
          eventDescription = Just $ makeDescription description,
          eventLocation = makeLocation . T.pack . show <$> mJitsiLink,
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
          eventSummary = Just $ makeSummary summary,
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
                ],
          eventAlarms =
            [ ICal.makeDisplayAlarm
                (ICal.makeDescription (T.unwords ["Meeting between", bookingSettingName, "and", bookingClientName, "starts in 15 minutes"]))
                ( ICal.TriggerDuration
                    ICal.AlarmTriggerRelationshipStart
                    ( ICal.DurationTime
                        ( ICal.DurTime
                            { durTimeSign = ICal.Negative,
                              durTimeHour = 0,
                              durTimeMinute = 15,
                              durTimeSecond = 0
                            }
                        )
                    )
                ),
              ICal.makeDisplayAlarm
                (ICal.makeDescription (T.unwords ["Meeting between", bookingSettingName, "and", bookingClientName, "starts in 5 minutes!"]))
                ( ICal.TriggerDuration
                    ICal.AlarmTriggerRelationshipStart
                    ( ICal.DurationTime
                        ( ICal.DurTime
                            { durTimeSign = ICal.Negative,
                              durTimeHour = 0,
                              durTimeMinute = 5,
                              durTimeSecond = 0
                            }
                        )
                    )
                )
            ]
        }
