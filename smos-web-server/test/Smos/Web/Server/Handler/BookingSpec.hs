{-# LANGUAGE OverloadedStrings #-}

module Smos.Web.Server.Handler.BookingSpec (spec) where

import Data.Time
import Data.Time.Zones.All
import ICal
import Smos.Web.Server.Foundation
import Smos.Web.Server.Handler.Booking
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
    it "is possible to fetch BookR for a user" $ \yc ->
      withAnyFreshAccount yc $ \username _ -> do
        logout
        get $ BookUserR username
        statusIs 200
  describe "makeICALCalendar" $
    it "produces the same calendar as before" $
      let bf =
            BookForm
              { bookFormClientName = "Example Client Name",
                bookFormClientEmailAddress = "client@example.com",
                bookFormUTCTime = UTCTime (fromGregorian 2023 06 22) (timeOfDayToTime (TimeOfDay 11 00 00)),
                bookFormUserTimeZone = Europe__Zurich,
                bookFormClientTimeZone = America__Denver,
                bookFormDuration = 30 * 60
              }

          now = UTCTime (fromGregorian 2023 04 22) (timeOfDayToTime (TimeOfDay 13 00 00))
       in pureGoldenTextFile "test_resources/booking/calendar.ics" (renderICalendar [makeICALCalendar now bf])
