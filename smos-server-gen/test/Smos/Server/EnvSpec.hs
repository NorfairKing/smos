{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.EnvSpec
  ( spec
  ) where

import Test.Hspec
import Test.Validity

import Control.Monad.IO.Class
import Data.Pool
import Database.Persist.Sqlite as DB

import Smos.Client

import Smos.Server.Env
import Smos.Server.TestUtils

spec :: Spec
spec =
  serverDBSpec $
  describe "writeServerStore" $ do
    it "can read exactly what was just written" $ \pool ->
      forAllValid $ \i ->
        forAllValid $ \serverStore -> do
          let uid = DB.toSqlKey i
          serverStore' <-
            testDB pool $ do
              writeServerStore uid serverStore
              readServerStore uid
          serverStore' `shouldBe` serverStore
    it "can read exactly what was just written, even if something else has been written first" $ \pool ->
      forAllValid $ \i ->
        forAllValid $ \serverStore1 ->
          forAllValid $ \serverStore2 -> do
            let uid = DB.toSqlKey i
            serverStore' <-
              testDB pool $ do
                writeServerStore uid serverStore1
                writeServerStore uid serverStore2
                readServerStore uid
            serverStore' `shouldBe` serverStore2

testDB :: Pool SqlBackend -> SqlPersistT IO a -> IO a
testDB = flip DB.runSqlPool
