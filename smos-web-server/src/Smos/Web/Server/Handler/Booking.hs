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
import Smos.Web.Server.Handler.Import

getBookingR :: Handler Html
getBookingR = withLogin $ \t -> do
  now <- liftIO getCurrentTime
  status <- runClientOrErr $ clientGetUserSubscription t
  let showBookingPage = status /= NotSubscribed
  withNavBar $ do
    $(widgetFile "booking")

getClientTZLabel :: Handler TZLabel
getClientTZLabel = do
  mTimezoneName <- lookupGetParam "timezone"
  let label = fromMaybe Etc__UTC $ mTimezoneName >>= (fromTZName . TE.encodeUtf8)
  pure label

getBookUserR :: Username -> Handler Html
getBookUserR username = do
  let allTzLabels :: [TZLabel]
      allTzLabels = [minBound .. maxBound]
  -- TODO: make the user timezone configurable
  let userTimeZoneLabel = Europe__Zurich
  let userTimeZone = tzByLabel userTimeZoneLabel

  clientTimeZoneLabel <- getClientTZLabel
  let clientTimeZone = tzByLabel clientTimeZoneLabel

  withNavBar $ do
    token <- genToken
    $(widgetFile "book-user/select-timezone")

data TimeZoneForm = TimeZoneForm {timeZoneFormTimeZone :: TZLabel}

timeZoneForm :: FormInput Handler TimeZoneForm
timeZoneForm =
  TimeZoneForm
    <$> ireq timeZoneLabelField "timezone"

getBookUserSlotR :: Username -> Handler Html
getBookUserSlotR username = do
  -- TODO: make the user timezone configurable
  let userTimeZoneLabel = Europe__Zurich
  let userTimeZone = tzByLabel userTimeZoneLabel

  TimeZoneForm {..} <- runInputGet timeZoneForm

  let clientTimeZoneLabel = timeZoneFormTimeZone
  let clientTimeZone = tzByLabel clientTimeZoneLabel

  BookingSlots {..} <- runClientOrErr $ clientGetBookingSlots username

  let timezones :: [(TZLabel, TZ)]
      timezones = map (\tzl -> (tzl, tzByLabel tzl)) [minBound .. maxBound]

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
  { bookFormUTCTime :: !UTCTime,
    bookFormUserTimeZone :: !TZLabel,
    bookFormClientTimeZone :: !TZLabel,
    bookFormDuration :: !NominalDiffTime
  }
  deriving (Show, Eq, Generic)

bookForm :: FormInput Handler BookForm
bookForm =
  BookForm
    <$> ( UTCTime
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

  withNavBar $ do
    token <- genToken
    $(widgetFile "book-user/booked")

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
  ( makeEvent
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
      eventOrganizer = Just ((mkOrganizer "mailto:client@example.com") {organizerCommonName = Just "Client common name"}),
      -- TODO more specific jitsi link
      eventURL = Just (URL "https://meet.jit.si/"),
      eventStatus = Just StatusTentative,
      -- TODO: Nice event summary
      eventSummary = Just (Summary "Nice Summary"),
      eventTransparency = TransparencyOpaque,
      eventAttendees =
        S.fromList
          -- TODO fill in user and client info
          [ (mkAttendee "mailto:user@example.com")
              { attendeeParticipationRole = ParticipationRoleRequiredParticipant,
                attendeeParticipationStatus = ParticipationStatusTentative,
                attendeeRSVPExpectation = RSVPExpectationTrue,
                attendeeCommonName = Just "User common name"
              },
            (mkAttendee "mailto:client@example.com")
              { attendeeParticipationRole = ParticipationRoleRequiredParticipant,
                attendeeParticipationStatus = ParticipationStatusTentative,
                attendeeRSVPExpectation = RSVPExpectationTrue,
                attendeeCommonName = Just "Client common name"
              }
          ]
    }
