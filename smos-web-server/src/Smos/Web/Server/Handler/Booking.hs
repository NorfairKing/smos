{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.Booking
  ( getBookingR,
    getBookUserR,
    getBookUserSlotR,
    postBookUserSlotR,
    BookForm (..),
    makeICALCalendar,
  )
where

import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Zones
import Data.Time.Zones.All
import GHC.Generics (Generic)
import ICal
import Network.URI
import Smos.Web.Server.Handler.Import

getBookingR :: Handler Html
getBookingR = withLogin $ \t -> do
  status <- runClientOrErr $ clientGetUserSubscription t
  let showBookingPage = status /= NotSubscribed
  withNavBar $ do
    $(widgetFile "booking")

getBookUserR :: Username -> Handler Html
getBookUserR username = do
  let allTzLabels :: [TZLabel]
      allTzLabels = [minBound .. maxBound]

  withNavBar $ do
    token <- genToken
    $(widgetFile "book-user/select-client")

data ClientForm = ClientForm
  { clientFormTimeZone :: !TZLabel,
    clientFormName :: !Text,
    clientFormEmailAddress :: !Text
  }

clientForm :: FormInput Handler ClientForm
clientForm =
  ClientForm
    <$> ireq timeZoneLabelField "timezone"
    <*> ireq textField "name"
    <*> ireq emailField "email-address"

getBookUserSlotR :: Username -> Handler Html
getBookUserSlotR username = do
  -- TODO: make the user timezone configurable
  let userTimeZoneLabel = Europe__Zurich
  let userTimeZone = tzByLabel userTimeZoneLabel

  ClientForm {..} <- runInputGet clientForm

  let clientTimeZoneLabel = clientFormTimeZone
  let clientTimeZone = tzByLabel clientTimeZoneLabel

  BookingSlots {..} <- runClientOrErr $ clientGetBookingSlots username

  let toClientLocalTime :: LocalTime -> LocalTime
      toClientLocalTime = utcToLocalTimeTZ clientTimeZone . localTimeToUTCTZ userTimeZone

  let clientOptions :: Map Day (Set (UTCTime, TimeOfDay, Day, TimeOfDay, NominalDiffTime))
      clientOptions =
        M.fromListWith S.union $
          map
            ( \(lt, dur) ->
                let LocalTime userDay userTod = lt
                    LocalTime clientDay clientTod = toClientLocalTime lt
                 in ( clientDay,
                      S.singleton
                        ( localTimeToUTCTZ userTimeZone lt,
                          clientTod,
                          userDay,
                          userTod,
                          dur
                        )
                    )
            )
            (M.toList bookingSlots)

  let formatDuration :: NominalDiffTime -> String
      formatDuration = formatTime defaultTimeLocale "%m min"

  withNavBar $ do
    token <- genToken
    $(widgetFile "book-user/select-slot")

data BookForm = BookForm
  { bookFormClientName :: !Text,
    bookFormClientEmailAddress :: !Text,
    bookFormUTCTime :: !UTCTime,
    bookFormUserTimeZone :: !TZLabel,
    bookFormClientTimeZone :: !TZLabel,
    bookFormDuration :: !NominalDiffTime
  }
  deriving (Show, Eq, Generic)

bookForm :: FormInput Handler BookForm
bookForm =
  BookForm
    <$> ireq textField "client-name"
    <*> ireq emailField "client-email-address"
    <*> ( UTCTime
            <$> ireq dayField "utc-day"
            <*> (timeOfDayToTime <$> ireq timeField "utc-time-of-day")
        )
    <*> ireq timeZoneLabelField "user-time-zone"
    <*> ireq timeZoneLabelField "client-time-zone"
    <*> ( (* 60) . (fromIntegral :: Int -> NominalDiffTime)
            <$> ireq intField "duration"
        )

tzLabelToText :: TZLabel -> Text
tzLabelToText = TE.decodeLatin1 . toTZName

timeZoneLabelField :: Field Handler TZLabel
timeZoneLabelField =
  checkMMap
    ( pure
        . ( \t -> case fromTZName t of
              Nothing ->
                Left $
                  T.pack $
                    unwords
                      [ "Unknown timezone: ",
                        show t
                      ]
              Just tz -> Right tz
          )
        . TE.encodeUtf8
    )
    tzLabelToText
    textField

postBookUserSlotR :: Username -> Handler Html
postBookUserSlotR username = do
  bf@BookForm {..} <- runInputPost bookForm
  liftIO $ print bf
  now <- liftIO getCurrentTime
  let ical = [makeICALCalendar now bf]
  let icalText = renderICalendar ical

  withNavBar $(widgetFile "book-user/booked")

makeICALCalendar ::
  UTCTime ->
  BookForm ->
  ICal.Calendar
makeICALCalendar now bf =
  (makeCalendar (ICal.ProdId "-//CS SYD//Smos//EN"))
    { calendarMethod = Just (ICal.Method "REQUEST"),
      calendarEvents =
        [ makeICALEvent now bf
        ]
    }

makeICALEvent ::
  UTCTime ->
  BookForm ->
  ICal.Event
makeICALEvent now BookForm {..} =
  let mClientURI = parseURIReference $ "mailto:" <> T.unpack bookFormClientEmailAddress
      mClientCalAddress = CalAddress <$> mClientURI
      clientCommonName = CommonName (QuotedParam bookFormClientName)
   in ( makeEvent
          -- TODO make a good UID
          (ICal.UID "uid here")
          (DateTimeStamp (DateTimeUTC now))
      )
        { eventDateTimeStart = Just $ DateTimeStartDateTime $ DateTimeUTC bookFormUTCTime,
          eventDateTimeEndDuration = Just (Right (nominalDiffTimeDuration bookFormDuration)),
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
          -- TODO more specific jitsi link
          eventURL = Just (URL "https://meet.jit.si/"),
          eventStatus = Just StatusTentative,
          -- TODO: Nice event summary
          eventSummary = Just (Summary "Nice Summary"),
          eventTransparency = TransparencyOpaque,
          eventAttendees =
            S.fromList $
              catMaybes
                -- TODO fill in user info
                [ Just
                    (mkAttendee "mailto:user@example.com")
                      { attendeeParticipationRole = ParticipationRoleRequiredParticipant,
                        attendeeParticipationStatus = ParticipationStatusTentative,
                        attendeeRSVPExpectation = RSVPExpectationTrue,
                        attendeeCommonName = Just "User common name"
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
