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
import Smos.Server.Handler.GetBookingSlots
import Smos.Server.Handler.Import
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet
import Text.Shakespeare.Text
import UnliftIO

servePostBooking :: Username -> Booking -> ServerHandler ICalendar
servePostBooking username booking = withUsernameId username $ \uid -> do
  mBookingConfig <- runDB $ getBy $ UniqueBookingConfigUser uid
  case mBookingConfig of
    Nothing -> throwError err404
    Just (Entity _ bookingConfig) -> do
      BookingSlots {..} <- computeBookingSlots uid bookingConfig
      let localTime = utcToLocalTimeTZ (tzByLabel (bookingConfigTimeZone bookingConfig)) (bookingUTCTime booking)
      if localTime `M.notMember` bookingSlots
        then throwError err400
        else do
          now <- liftIO getCurrentTime
          uuid <- nextRandomUUID
          let ical = [makeICALCalendar now uuid bookingConfig booking]
          sendBookingEmail bookingConfig booking ical
          pure ical

sendBookingEmail :: BookingConfig -> Booking -> ICal.ICalendar -> ServerHandler ()
sendBookingEmail bookingConfig@BookingConfig {..} booking@Booking {..} ical = do
  mSendAddress <- asks serverEnvBookingEmailAddress
  case mSendAddress of
    Nothing ->
      logWarnN $
        T.pack $
          unwords
            [ "Not sending booking email because no send address has been configured from",
              show bookingClientEmailAddress,
              "to",
              show bookingConfigEmailAddress
            ]
    Just sendEmailAddress -> do
      logInfoN $
        T.pack $
          unwords
            [ "Sending booking email from",
              show bookingClientEmailAddress,
              "to",
              show bookingConfigEmailAddress
            ]

      let mail = makeBookingEmail sendEmailAddress bookingConfig booking ical
      rawEmailBs <- liftIO $ LB.toStrict <$> renderMail' mail

      let rawMessage = SES.newRawMessage rawEmailBs
      let request = SES.newSendRawEmail rawMessage

      logFunc <- askLoggerIO
      errOrResponse <- liftIO $ runLoggingT (runAWS request) logFunc

      case SES.httpStatus <$> errOrResponse of
        Right 200 -> pure ()
        _ -> throwError $ err500 {errBody = "Failed to send email."}

makeBookingEmail :: Text -> BookingConfig -> Booking -> ICalendar -> Mail
makeBookingEmail sendEmailAddress bookingConfig@BookingConfig {..} booking@Booking {..} ical =
  let sendAddress = Address {addressName = Nothing, addressEmail = sendEmailAddress}
      toAddress =
        Address
          { addressName = Just bookingConfigName,
            addressEmail = bookingConfigEmailAddress
          }
      ccAddress =
        Address
          { addressName = Just bookingClientName,
            addressEmail = bookingClientEmailAddress
          }
      subject = makeEmailSubject bookingConfig booking
      htmlContent = makeEmailHtml bookingConfig booking
      textContent = makeEmailText bookingConfig booking
      icalBS = renderICalendarByteString ical
   in addAttachmentBS "text/calendar" "invitation.ics" (LB.fromStrict icalBS)
        $ addPart
          [ htmlPart $ LT.fromStrict htmlContent,
            plainPart $ LT.fromStrict textContent
          ]
        $ (emptyMail sendAddress)
          { mailTo = [toAddress],
            mailCc = [ccAddress],
            mailHeaders =
              [ ("Subject", subject),
                ("Reply-To", renderAddress sendAddress)
              ]
          }

runAWS ::
  ( MonadUnliftIO m,
    MonadLoggerIO m,
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

makeEmailSubject :: BookingConfig -> Booking -> Text
makeEmailSubject BookingConfig {..} Booking {..} =
  T.pack $
    unwords
      [ "Smos Booking: Calendar invite for",
        T.unpack bookingConfigName,
        "from",
        T.unpack bookingClientName
      ]

makeEmailHtml :: BookingConfig -> Booking -> Text
makeEmailHtml BookingConfig {..} Booking {..} =
  LT.toStrict $ renderHtml $(shamletFile "templates/booking.hamlet")

makeEmailText :: BookingConfig -> Booking -> Text
makeEmailText BookingConfig {..} Booking {..} =
  LT.toStrict $(stextFile "templates/booking.txt")

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
            Booking _ _ _ _ _ _ = undefined
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
