{-# LANGUAGE OverloadedStrings #-}

module Smos.Sync.Client.RegisterSpec
  ( spec
  ) where

import GHC.Generics (Generic)

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Validity
import Test.Validity.Aeson

import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Mergeful as Mergeful
import qualified Data.Mergeful.Timed as Mergeful
import Data.Set (Set)
import qualified Data.Set as S

import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader

import Path
import Servant.Client

import Database.Persist.Sqlite as DB

import Smos.Sync.API
import Smos.Sync.Client.Env
import Smos.Sync.Client.Sync
import Smos.Sync.Client.Sync.Gen ()
import Smos.Sync.Server.TestUtils

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
