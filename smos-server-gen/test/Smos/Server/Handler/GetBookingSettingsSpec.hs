{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.GetBookingSettingsSpec (spec) where

import qualified Network.HTTP.Types as HTTP
import Smos.Client
import Smos.Data.Gen ()
import Smos.Server.InterestingStore
import Smos.Server.TestUtils
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec =
  serverSpec $ do
    -- We must return a 404 to not leak account existence for people who have not activated booking.
    it "gets a 404 when a user has not activated booking" $ \cenv ->
      withNewUserAndData cenv $ \Register {..} _ -> do
        errOrBookingSettings <- runClient cenv $ clientGetBookingSettings registerUsername
        case errOrBookingSettings of
          Left err -> case err of
            FailureResponse _ response ->
              responseStatusCode response `shouldBe` HTTP.notFound404
            _ -> expectationFailure "should have gotten a 404 but got a different error instead."
          Right _ -> expectationFailure "Should have gotten a 404."

    it "gets the settings it put" $ \cenv ->
      forAllValid $ \store ->
        forAllValid $ \bookingSettings ->
          withNewUserAndData cenv $ \Register {..} token -> do
            bookingSettings' <- testClient cenv $ do
              setupInterestingStore token (addBookingSettingsToInterestingStore bookingSettings store)
              clientGetBookingSettings registerUsername
            bookingSettings' `shouldBe` bookingSettings
