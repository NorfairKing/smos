{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Smos.Web.Server.Handler.Booking
  ( getBookingR,
    postBookingR,
    getBookUserR,
    getBookUserSlotR,
    getBookUserDetailsR,
    postBookUserDetailsR,
  )
where

import Control.Arrow (left)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time.Zones
import Data.Time.Zones.All
import GHC.Generics (Generic)
import ICal
import Smos.Data
import Smos.Web.Server.Handler.Import

getBookingR :: Handler Html
getBookingR = withLogin' $ \username t -> do
  status <- runClientOrErr $ clientGetUserSubscription t
  let showBookingPage = status /= NotSubscribed

  mBookingSettings <- runClientOrNotFound $ clientGetBookingSettings username

  let allTzLabels :: [TZLabel]
      allTzLabels = [minBound .. maxBound]

  withNavBar $ do
    token <- genToken
    $(widgetFile "booking")

bookingSettingsForm :: FormInput Handler BookingSettings
bookingSettingsForm =
  BookingSettings
    <$> ireq textField "name"
    <*> ireq emailField "email-address"
    <*> ireq timeZoneLabelField "timezone"

postBookingR :: Handler Html
postBookingR = withLogin $ \t -> do
  bs <- runInputPost bookingSettingsForm

  NoContent <- runClientOrErr $ clientPutBookingSettings t bs

  redirect BookingR

getBookUserR :: Username -> Handler Html
getBookUserR username = do
  mBookingSettings <- runClientOrNotFound $ clientGetBookingSettings username
  case mBookingSettings of
    Nothing -> notFound
    Just _ -> do
      let allTzLabels :: [TZLabel]
          allTzLabels = [minBound .. maxBound]

      withNavBar $ do
        token <- genToken
        $(widgetFile "book-user/select-timezone")

data ClientForm = ClientForm
  { clientFormTimeZone :: !TZLabel
  }

clientForm :: FormInput Handler ClientForm
clientForm =
  ClientForm
    <$> ireq timeZoneLabelField "timezone"

getBookUserSlotR :: Username -> Handler Html
getBookUserSlotR username = do
  BookingSettings {..} <- runClientOrErr $ clientGetBookingSettings username

  let userTimeZoneLabel = bookingSettingTimeZone
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

data ChosenSlot = ChosenSlot
  { chosenSlotClientTimeZone :: !TZLabel,
    chosenSlotUTCTime :: !UTCTime,
    chosenSlotDuration :: !NominalDiffTime
  }
  deriving (Show, Eq, Generic)

chosenSlotForm :: FormInput Handler ChosenSlot
chosenSlotForm =
  ChosenSlot
    <$> ireq timeZoneLabelField "client-timezone"
    <*> ( UTCTime
            <$> ireq dayField "utc-day"
            <*> (timeOfDayToTime <$> ireq timeField "utc-time-of-day")
        )
    <*> ( (* 60) . (fromIntegral :: Int -> NominalDiffTime)
            <$> ireq intField "duration"
        )

getBookUserDetailsR :: Username -> Handler Html
getBookUserDetailsR username = do
  BookingSettings {..} <- runClientOrErr $ clientGetBookingSettings username

  ChosenSlot {..} <- runInputGet chosenSlotForm

  withNavBar $ do
    token <- genToken
    $(widgetFile "book-user/add-info")

bookingForm :: FormInput Handler Booking
bookingForm =
  Booking
    <$> ireq textField "client-name"
    <*> ireq emailField "client-email-address"
    <*> ireq timeZoneLabelField "client-timezone"
    <*> ( UTCTime
            <$> ireq dayField "utc-day"
            <*> (timeOfDayToTime <$> ireq timeField "utc-time-of-day")
        )
    <*> ( (* 60) . (fromIntegral :: Int -> NominalDiffTime)
            <$> ireq intField "duration"
        )
    <*> (fmap unTextarea <$> iopt textareaField "extra-info")

postBookUserDetailsR :: Username -> Handler Html
postBookUserDetailsR username = do
  BookingSettings {..} <- runClientOrErr $ clientGetBookingSettings username

  booking@Booking {..} <- runInputPost bookingForm

  ical <- runClientOrErr $ clientPostBooking username booking

  let icalText = renderICalendar ical

  withNavBar $(widgetFile "book-user/booked")

timeZoneLabelField :: Field Handler TZLabel
timeZoneLabelField =
  checkMMap
    (pure . left T.pack . parseTZLabel)
    renderTZLabel
    textField
