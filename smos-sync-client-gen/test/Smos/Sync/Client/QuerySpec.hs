module Smos.Sync.Client.QuerySpec
  ( spec
  ) where

import Test.Hspec
import Test.Validity

import Control.Monad.IO.Class
import Data.Pool
import Database.Persist.Sqlite as DB
import Path
import Path.IO

import System.Environment

import Servant.Client

import Smos.API

import Smos.Server.TestUtils

import Smos.Sync.Client
import Smos.Sync.Client.Query
import Smos.Sync.Client.TestUtils

spec :: Spec
spec =
  clientDBSpec $
  describe "writeClientMetadata" $ do
    it "can read exactly what was just written" $ \pool ->
      forAllValid $ \m -> do
        m' <-
          testDB pool $ do
            writeClientMetadata m
            readClientMetadata
        m' `shouldBe` m
    it "can read exactly what was just written, even if something else has been written first" $ \pool ->
      forAllValid $ \m1 ->
        forAllValid $ \m2 -> do
          m' <-
            testDB pool $ do
              writeClientMetadata m1
              writeClientMetadata m2
              readClientMetadata
          m' `shouldBe` m2

testDB :: Pool SqlBackend -> SqlPersistT IO a -> IO a
testDB = flip DB.runSqlPool
