{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.Handler.PostSyncSpec
  ( spec,
  )
where

import Smos.Client
import Smos.Server.InterestingStore
import Smos.Server.TestUtils
import Test.Hspec
import Test.Validity
import Text.Show.Pretty

spec :: Spec
spec =
  serverSpec $ describe "PostSync" $ do
    it "produces valid resuls" $ \cenv ->
      forAllValid $ \request ->
        withNewUser cenv $ \t -> do
          response <- testClientOrErr cenv (clientPostSync t request)
          shouldBeValid response
    xit "is idempotent after an arbitrary setup request" $ \cenv ->
      forAllValid $ \interestingStore ->
        forAllValid $ \request ->
          withNewUser cenv $ \t -> do
            pPrint interestingStore
            print ("Setup" :: String)
            testClientOrErr cenv $ setupInterestingStore t interestingStore
            pPrint request
            print ("request1" :: String)
            response1 <- testClientOrErr cenv (clientPostSync t request)
            pPrint request
            print ("request2" :: String)
            response2 <- testClientOrErr cenv (clientPostSync t request)
            response2 `shouldBe` response1
