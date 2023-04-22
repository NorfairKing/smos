{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.GetBookingSettingsSpec (spec) where

import Smos.Client
import Smos.Data.Gen ()
import Smos.Server.InterestingStore
import Smos.Server.TestUtils
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec =
  serverSpec $
    it "gets a 404 when a user has not activated booking" $ \cenv ->
      withNewUser cenv $ \token -> do
        errOrBookingSettings <- runClient cenv $ clientGetBookingSettings token
        case errOrBookingSettings of
          Left err -> pure ()
          Right _ -> expectationFailure "Should have gotten a 404."
