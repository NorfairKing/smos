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
  dbSpec $
  describe "writeServerStore" $
  it "produces valid resuls" $ \pool ->
    forAllValid $ \i ->
      forAllValid $ \serverStore -> do
        let uid = DB.toSqlKey i
        serverStore' <-
          testDB pool $ do
            writeServerStore uid serverStore
            readServerStore uid
        serverStore' `shouldBe` serverStore

testDB :: Pool SqlBackend -> SqlPersistT IO a -> IO a
testDB = flip DB.runSqlPool
