{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.Handler.DeleteSmosFileSpec
  ( spec,
  )
where

import qualified Network.HTTP.Types as HTTP
import Smos.Client
import Smos.Data.Gen ()
import Smos.Server.TestUtils
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec =
  serverSpec $ do
    it "cannot delete a file that does not exist" $ \cenv ->
      forAllValid $ \path ->
        withNewUser cenv $ \t -> do
          errOrNoContent <- runClient cenv $ clientDeleteSmosFile t path
          case errOrNoContent of
            Left err -> case err of
              FailureResponse _ response ->
                responseStatusCode response `shouldBe` HTTP.notFound404
              _ -> expectationFailure "should have gotten a 404 but got a different error instead."
            Right _ -> expectationFailure "should not have succeeded."

    it "deletes a smos file that was put with PutSmosFile, after which it no longer exists" $ \cenv ->
      forAllValid $ \path ->
        forAllValid $ \sf ->
          withNewUser cenv $ \t -> do
            testClient cenv $ do
              NoContent <- clientPutSmosFile t path sf
              NoContent <- clientDeleteSmosFile t path
              pure ()
            errOrSmosFile <- runClient cenv $ clientGetSmosFile t path
            case errOrSmosFile of
              Left err -> case err of
                FailureResponse _ response ->
                  responseStatusCode response `shouldBe` HTTP.notFound404
                _ -> expectationFailure "should have gotten a 404 but got a different error instead."
              Right _ -> expectationFailure "should not have succeeded."
