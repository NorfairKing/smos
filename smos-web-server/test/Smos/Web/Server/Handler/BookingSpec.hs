{-# LANGUAGE OverloadedStrings #-}

module Smos.Web.Server.Handler.BookingSpec (spec) where

import Control.Monad.Reader
import qualified Data.Map as M
import qualified Data.Mergeful as Mergeful
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Zones.All
import Data.Yaml as Yaml
import Smos.API
import Smos.Client
import Smos.Server.TestUtils
import Smos.Web.Server.Foundation
import Smos.Web.Server.TestUtils
import Test.Syd
import Test.Syd.Yesod

spec :: Spec
spec = do
  smosWebServerSpec $ do
    it "is possible to fetch BookingsR and get a 200 response after loggin in" $ \yc ->
      withAnyFreshAccount_ yc $ do
        get BookingR
        statusIs 200

    it "GETs a 404 for BookR for a user that doesn't have booking activated" $ \yc ->
      withAnyFreshAccount yc $ \username _ -> do
        logout
        get $ BookUserR username
        statusIs 404

    it "GETs a 200 for BookR for a user that has booking activated" $ \yc ->
      withAnyFreshAccount yc $ \username password -> do
        setupBookingSettings username password dummyBookingSettings
        get $ BookUserR username
        statusIs 200

    it "GETs a 200 for the booking slots" $ \yc ->
      withAnyFreshAccount yc $ \username password -> do
        setupBookingSettings username password dummyBookingSettings

        request $ do
          setMethod "GET"
          setUrl $ BookUserSlotR username
          addGetParam "timezone" "UTC"
          addGetParam "slot-size" "30"
        statusIs 200

    it "GETs a 200 for the details" $ \yc ->
      withAnyFreshAccount yc $ \username password -> do
        setupBookingSettings username password dummyBookingSettings

        now <- liftIO getCurrentTime
        let today = utctDay now
        let tomorrow = addDays 1 today
        request $ do
          setMethod "GET"
          setUrl $ BookUserDetailsR username
          addGetParam "client-timezone" "UTC"
          addGetParam "duration" "30"
          addGetParam "utc-day" $ T.pack $ formatTime defaultTimeLocale "%F" tomorrow
          addGetParam "utc-time-of-day" $ T.pack $ formatTime defaultTimeLocale "%H:%M" $ TimeOfDay 16 00 00
        statusIs 200

    it "POSTs a 200 for a succesful booking" $ \yc ->
      withAnyFreshAccount yc $ \username password -> do
        setupBookingSettings username password dummyBookingSettings
        get $ BookUserR username
        statusIs 200

        now <- liftIO getCurrentTime
        let today = utctDay now
        let tomorrow = addDays 1 today
        request $ do
          setMethod "POST"
          setUrl $ BookUserDetailsR username
          addPostParam "client-name" "Jane"
          addPostParam "client-email-address" "jane@example.com"
          addPostParam "client-timezone" "UTC"
          addPostParam "duration" "30"
          addPostParam "utc-day" $ T.pack $ formatTime defaultTimeLocale "%F" tomorrow
          addPostParam "utc-time-of-day" $ T.pack $ formatTime defaultTimeLocale "%H:%M" $ TimeOfDay 16 00 00
        statusIs 200

    it "POSTs a 200 for a succesful booking even with an annoying name" $ \yc ->
      withAnyFreshAccount yc $ \username password -> do
        setupBookingSettings username password dummyBookingSettings
        get $ BookUserR username
        statusIs 200

        now <- liftIO getCurrentTime
        let today = utctDay now
        let tomorrow = addDays 1 today
        request $ do
          setMethod "POST"
          setUrl $ BookUserDetailsR username
          addPostParam "client-name" "Jane \"Foster\" Doe"
          addPostParam "client-email-address" "jane@example.com"
          addPostParam "client-timezone" "UTC"
          addPostParam "duration" "30"
          addPostParam "utc-day" $ T.pack $ formatTime defaultTimeLocale "%F" tomorrow
          addPostParam "utc-time-of-day" $ T.pack $ formatTime defaultTimeLocale "%H:%M" $ TimeOfDay 16 00 00
        statusIs 200

dummyBookingSettings :: BookingSettings
dummyBookingSettings =
  BookingSettings
    { bookingSettingName = "John Doe",
      bookingSettingEmailAddress = "user@example.com",
      bookingSettingTimeZone = UTC,
      bookingSettingAllowedDurations = S.singleton 30,
      bookingSettingMinimumDaysAhead = 0,
      bookingSettingMaximumDaysAhead = 7,
      bookingSettingEarliestTimeOfDay = TimeOfDay 9 0 0,
      bookingSettingLatestTimeOfDay = TimeOfDay 17 0 0,
      bookingSettingAllowedDays = S.fromList [Monday, Tuesday, Wednesday, Thursday, Friday]
    }

setupBookingSettings :: Username -> Text -> BookingSettings -> YesodClientM App ()
setupBookingSettings username password bookingSettings = do
  cenv <- asks $ appAPIClientEnv . yesodClientSite
  let syncRequest =
        SyncRequest
          { syncRequestItems =
              Mergeful.initialSyncRequest
                { Mergeful.syncRequestNewItems =
                    M.singleton
                      bookingFilePath
                      SyncFile {syncFileContents = Yaml.encode bookingSettings}
                }
          }
  _ <- liftIO $
    testClient cenv $ do
      errOrToken <- clientLogin Login {loginUsername = username, loginPassword = password}
      case errOrToken of
        Left err -> liftIO $ expectationFailure $ show err
        Right token -> clientPostSync token syncRequest
  pure ()
