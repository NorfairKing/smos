{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.GetBookingSlotsSpec (spec) where

import Smos.Client
import Smos.Data.Gen ()
import Smos.Server.InterestingStore
import Smos.Server.TestUtils
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec =
  serverSpec $
    it "produces a valid smos directory forest for any interesting store" $ \cenv ->
      forAllValid $ \store ->
        withNewUserAndData cenv $ \Register {..} t -> do
          runClientOrDie cenv $ setupInterestingStore t store
          bookingSlots <- testClient cenv $ clientGetBookingSlots registerUsername
          shouldBeValid bookingSlots
