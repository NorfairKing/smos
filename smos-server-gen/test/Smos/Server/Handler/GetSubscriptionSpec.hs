{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.GetSubscriptionSpec
  ( spec,
  )
where

import Smos.Client
import Smos.Data.Gen ()
import Smos.Server.TestUtils
import Test.Syd

spec :: Spec
spec =
  describe "GetSubscription" $
    serverSpec $
      it "gets that the user is not subscribed for a new user" $ \cenv ->
        withNewUser cenv $ \t -> do
          UserSubscription {..} <- testClient cenv $ clientGetUserSubscription t
          userSubscriptionEnd `shouldBe` Nothing
