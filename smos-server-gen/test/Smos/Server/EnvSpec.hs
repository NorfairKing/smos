{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.EnvSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck
import Test.Validity

import qualified Data.Map as M

import Data.Mergeful.Collection (ServerStore(..), initialServerStore)
import Data.Pool
import Database.Persist.Sqlite as DB

import Smos.Server.Env
import Smos.Server.TestUtils

spec :: Spec
spec =
  serverDBSpec $
  describe "writeServerStore" $ do
    it "can read an empty store" $ \pool ->
      forAllValid $ \i -> do
        let uid = DB.toSqlKey i
        serverStore' <- testDB pool $ readServerStore uid
        serverStore' `shouldBe` initialServerStore
    it "can write an empty store" $ \pool ->
      forAllValid $ \i -> do
        let uid = DB.toSqlKey i
        serverStore' <-
          testDB pool $ do
            writeServerStore uid initialServerStore
            readServerStore uid
        serverStore' `shouldBe` initialServerStore
    it "can read exactly what was just written" $ \pool ->
      forAllValid $ \i ->
        forAllValid $ \serverStore -> do
          let uid = DB.toSqlKey i
          print uid
          print $ M.size $ serverStoreItems serverStore
          print $ M.keys $ serverStoreItems serverStore
          print $ M.elems $ serverStoreItems serverStore
          print serverStore
          serverStore' <-
            testDB pool $ do
              writeServerStore uid serverStore
              readServerStore uid
          serverStore' `shouldBe` serverStore
    it "can read exactly what was just written, even if something else has been written first" $ \pool ->
      forAllValid $ \i ->
        forAllValid $ \serverStore1 ->
          forAll
            (genValid `suchThat`
             (\ss2 -> serverStoreItems serverStore1 `M.intersection` serverStoreItems ss2 == M.empty)) $ \serverStore2 -> do
            let uid = DB.toSqlKey i
            serverStore' <-
              testDB pool $ do
                writeServerStore uid serverStore1
                writeServerStore uid serverStore2
                readServerStore uid
            serverStore' `shouldBe` serverStore2

testDB :: Pool SqlBackend -> SqlPersistT IO a -> IO a
testDB = flip DB.runSqlPool
