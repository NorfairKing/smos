{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.Handler.PostSyncSpec
  ( spec,
  )
where

import Data.Mergeful
import Smos.Client
import Smos.Server.TestUtils
import Test.Hspec
import Test.Validity

spec :: Spec
spec =
  serverSpec $ describe "PostSync" $ do
    it "produces valid resuls" $ \cenv ->
      forAllValid $ \request ->
        withNewUser cenv $ \t -> do
          response <- testClientOrErr cenv (clientPostSync t request)
          shouldBeValid response
    it "is idempotent after an arbitrary setup request" $ \cenv ->
      forAllValid $ \initial ->
        forAllValid $ \request ->
          withNewUser cenv $ \t -> do
            let initialRequest = initialSyncRequest {syncRequestNewItems = initial}
            _ <- testClientOrErr cenv (clientPostSync t initialRequest)
            response1 <- testClientOrErr cenv (clientPostSync t request)
            response2 <- testClientOrErr cenv (clientPostSync t request)
            response2 `shouldBe` response1
