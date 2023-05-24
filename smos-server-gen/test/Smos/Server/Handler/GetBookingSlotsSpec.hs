{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.GetBookingSlotsSpec (spec) where

import qualified Data.Set as S
import Network.HTTP.Types as HTTP
import Smos.Client
import Smos.Data.Gen ()
import Smos.Server.InterestingStore
import Smos.Server.TestUtils
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec =
  serverSpec $ do
    it "cannot get booking slots for a user that has not activated booking" $ \cenv ->
      withNewUserAndData cenv $ \Register {..} _ -> do
        errOrNoBookingSlots <- runClient cenv $ clientGetBookingSlots registerUsername (30 * 60)
        case errOrNoBookingSlots of
          Left err -> case err of
            FailureResponse _ response ->
              responseStatusCode response `shouldBe` HTTP.notFound404
            _ -> expectationFailure "should have gotten a 404 but got a different error instead."
          Right _ -> expectationFailure "should not have succeeded."

    it "produces valid booking slots for any interesting store" $ \cenv ->
      forAllValid $ \bookingSettings ->
        forAll (elements (S.toList (bookingSettingAllowedDurations bookingSettings))) $ \duration ->
          forAllValid $ \store ->
            withNewUserAndData cenv $ \Register {..} token -> do
              runClientOrDie cenv $ setupInterestingStore token (addBookingSettingsToInterestingStore bookingSettings store)
              bookingSlots <- testClient cenv $ clientGetBookingSlots registerUsername duration
              shouldBeValid bookingSlots
