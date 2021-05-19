{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.Handler.GetMonetisationSpec
  ( spec,
  )
where

import Smos.API as API
import Smos.Client
import Smos.Server.TestUtils
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec =
  serverSpec $
    describe "GetMonetisation" $
      it "gets monetisation info that says thet the server is not monetised" $ \cenv -> do
        mm <- testClient cenv clientGetMonetisation
        mm `shouldBe` Nothing
