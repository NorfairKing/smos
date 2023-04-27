{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.GetBookingSlotsSpec (spec) where

import Network.HTTP.Types as HTTP
import Smos.Client
import Smos.Data.Gen ()
import Smos.Server.InterestingStore
import Smos.Server.TestUtils
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec =
  serverSpec $ do
    it "cannot get booking slots for a user that has not activated booking" $ \cenv ->
      withNewUserAndData cenv $ \Register {..} _ -> do
        errOrNoBookingSlots <- runClient cenv $ clientGetBookingSlots registerUsername
        case errOrNoBookingSlots of
          Left err -> case err of
            FailureResponse _ response ->
              responseStatusCode response `shouldBe` HTTP.notFound404
            _ -> expectationFailure "should have gotten a 404 but got a different error instead."
          Right _ -> expectationFailure "should not have succeeded."

    it "produces valid booking slots for any interesting store" $ \cenv ->
      forAllValid $ \bookingSettings ->
        forAllValid $ \store ->
          withNewUserAndData cenv $ \Register {..} token -> do
            testClient cenv $ setupInterestingStore token store
            NoContent <- testClient cenv $ clientPutBookingSettings token bookingSettings
            bookingSlots <- testClient cenv $ clientGetBookingSlots registerUsername
            shouldBeValid bookingSlots
