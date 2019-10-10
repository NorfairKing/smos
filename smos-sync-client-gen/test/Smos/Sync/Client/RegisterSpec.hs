{-# LANGUAGE OverloadedStrings #-}

module Smos.Sync.Client.RegisterSpec
  ( spec
  ) where

import Test.Hspec
import Test.Validity

import Servant.Client

import Smos.Client
import Smos.Server.TestUtils

import Smos.Sync.Client.Sync.Gen ()

spec :: Spec
spec =
  serverSpec $
  describe "PostRegister" $
  it "works for any username and password" $ \cenv ->
    forAllValid $ \register -> do
      NoContent <- testClientOrErr cenv (clientPostRegister register)
      pure ()

testClient :: ClientEnv -> ClientM a -> IO (Either ClientError a)
testClient = flip runClientM

testClientOrErr :: ClientEnv -> ClientM a -> IO a
testClientOrErr cenv func = do
  res <- testClient cenv func
  case res of
    Left err -> do
      expectationFailure $ show err
      undefined
    Right r -> pure r
