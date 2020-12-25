{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.PutSmosFileSpec
  ( spec,
  )
where

import qualified Data.Mergeful as Mergeful
import Path
import Smos.Client
import Smos.Data.Gen ()
import Smos.Server.DB
import Smos.Server.TestUtils
import Test.Syd
import Test.Syd.Servant
import Test.Syd.Validity

spec :: Spec
spec =
  describe "PutSmosFile" $ do
    serverSpec $
      it "puts a smos file that can be fetched with GetSmosFile" $
        \cenv ->
          forAllValid $ \path ->
            forAllValid $ \sf ->
              withNewUser cenv $ \t -> do
                actual <- testClient cenv $ do
                  NoContent <- clientPutSmosFile t path sf
                  clientGetSmosFile t path
                actual `shouldBe` sf
    serverEnvSpec $
      it "updates the server time" $ \senv ->
        forAllValid $ \path ->
          forAllValid $ \sf1 ->
            forAllValid $ \sf2 -> withTestEnv senv $
              withServerEnvNewUser $ \t -> do
                NoContent <- serverEnvClientOrErr $ clientPutSmosFile t path sf1
                NoContent <- serverEnvClientOrErr $ clientPutSmosFile t path sf2
                mServerFile <- serverEnvDB $ selectFirst [ServerFilePath ==. path] []
                liftIO $ case mServerFile of
                  Nothing -> expectationFailure $ "We expected a server file at: " <> fromRelFile path
                  Just (Entity _ ServerFile {..}) -> serverFileTime `shouldBe` Mergeful.ServerTime 1
