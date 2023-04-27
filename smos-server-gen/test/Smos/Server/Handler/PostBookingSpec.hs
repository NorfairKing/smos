{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.PostBookingSpec (spec) where

import qualified Data.Map as M
import Data.Time
import Data.Time.Zones
import Data.Time.Zones.All
import qualified Data.UUID as UUID
import qualified Data.UUID.Typed as Typed
import ICal
import qualified Network.HTTP.Types as HTTP
import Safe
import Smos.Client
import Smos.Data.Gen ()
import Smos.Server.DB
import Smos.Server.Handler.PostBooking
import Smos.Server.InterestingStore
import Smos.Server.TestUtils
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  serverSpec $ do
    it "cannot post a booking for a user that has not activated booking" $ \cenv ->
      forAllValid $ \booking ->
        withNewUserAndData cenv $ \Register {..} _ -> do
          errOrNoContent <- runClient cenv $ clientPostBooking registerUsername booking
          case errOrNoContent of
            Left err -> case err of
              FailureResponse _ response ->
                responseStatusCode response `shouldBe` HTTP.notFound404
              _ -> expectationFailure "should have gotten a 404 but got a different error instead."
            Right _ -> expectationFailure "should not have succeeded."

    it "can post a booking for any time slot" $ \cenv ->
      forAllValid $ \bookingSettings ->
        forAllValid $ \bookingPrototype ->
          forAllValid $ \slotChoice ->
            forAllValid $ \store ->
              withNewUserAndData cenv $ \Register {..} token -> do
                runClientOrDie cenv $ setupInterestingStore token store
                NoContent <- testClient cenv $ clientPutBookingSettings token bookingSettings
                BookingSettings {..} <- testClient cenv $ clientGetBookingSettings registerUsername
                BookingSlots {..} <- testClient cenv $ clientGetBookingSlots registerUsername
                let ls = M.toList bookingSlots
                if null ls
                  then pure () -- Can't choose one if there are none
                  else case ls `atMay` (fromIntegral (slotChoice :: Word) `mod` length ls) of
                    Nothing -> pure () -- Can't choose one if there are none
                    Just (localTime, duration) -> do
                      let booking =
                            bookingPrototype
                              { bookingUTCTime = localTimeToUTCTZ (tzByLabel bookingSettingTimeZone) localTime,
                                bookingDuration = duration
                              }
                      ical <- testClient cenv $ clientPostBooking registerUsername booking
                      shouldBeValid ical

    it "cannot post a booking for any non time slot" $ \cenv ->
      forAllValid $ \bookingSettings ->
        forAllValid $ \booking ->
          withNewUserAndData cenv $ \Register {..} token -> do
            NoContent <- testClient cenv $ clientPutBookingSettings token bookingSettings
            errOrNoContent <- runClient cenv $ clientPostBooking registerUsername booking
            case errOrNoContent of
              Left err -> case err of
                FailureResponse _ response ->
                  responseStatusCode response `shouldBe` HTTP.badRequest400
                _ -> expectationFailure "should have gotten a 400 but got a different error instead."
              Right _ -> expectationFailure "should not have succeeded."

  describe "Golden" $ do
    let now = UTCTime (fromGregorian 2023 04 22) (timeOfDayToTime (TimeOfDay 13 00 00))
        uuid = Typed.UUID $ UUID.fromWords 1 2 3 4
        bc =
          BookingConfig
            { bookingConfigUser = toSqlKey 0,
              bookingConfigName = "Example User Name",
              bookingConfigEmailAddress = "user@example.com",
              bookingConfigTimeZone = Europe__Zurich
            }
        b =
          Booking
            { bookingClientName = "Example Client Name",
              bookingClientEmailAddress = "client@example.com",
              bookingUTCTime = UTCTime (fromGregorian 2023 06 22) (timeOfDayToTime (TimeOfDay 11 00 00)),
              bookingClientTimeZone = America__Denver,
              bookingDuration = 30 * 60
            }

    describe "makeEmailSubject" $
      it "produces the same email subject as before" $
        pureGoldenTextFile "test_resources/booking/subject.txt" $
          makeEmailSubject bc b

    describe "makeEmailText" $
      it "produces the same email text as before" $
        pureGoldenTextFile "test_resources/booking/email.txt" $
          makeEmailText bc b

    describe "makeEmailHtml" $
      it "produces the same email html as before" $
        pureGoldenTextFile "test_resources/booking/email.html" $
          makeEmailHtml bc b

    describe "makeICALCalendar" $
      it "produces the same calendar as before" $
        pureGoldenTextFile "test_resources/booking/calendar.ics" (renderICalendar [makeICALCalendar now uuid bc b])
