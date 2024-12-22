{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Smos.Sync.Client.Sync.MetaSpec
  ( spec,
  )
where

import qualified Data.Mergeful as Mergeful
import Data.Pool
import Database.Persist.Sqlite as DB
import Path
import Smos.API.SHA256 as SHA256
import Smos.Sync.Client.DB
import Smos.Sync.Client.Meta
import Smos.Sync.Client.MetaMap.Gen ()
import Smos.Sync.Client.TestUtils
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  clientDBSpec $
    describe "writeClientMetadata" $ do
      it "can read exactly what was just written" $ \pool ->
        forAllValid $ \m -> do
          m' <- testDB pool $ do
            writeClientMetadata m
            readClientMetadata
          m' `shouldBe` m
      it "can read exactly what was just written, even if something else has been written first" $ \pool ->
        forAllValid $ \m1 ->
          forAllValid $ \m2 -> do
            m' <- testDB pool $ do
              writeClientMetadata m1
              writeClientMetadata m2
              readClientMetadata
            m' `shouldBe` m2
      it "does not crash while reading this nasty situation" $ \pool -> do
        m <- testDB pool $ do
          insertMany_
            [ ClientFile
                { clientFilePath = [relfile|foo|],
                  clientFileSha256 = SHA256.hashBytes "abc",
                  clientFileTime = Mergeful.initialServerTime
                },
              ClientFile
                { clientFilePath = [relfile|foo/bar|],
                  clientFileSha256 = SHA256.hashBytes "cde",
                  clientFileTime = Mergeful.initialServerTime
                }
            ]
          readClientMetadata
        shouldBeValid m

testDB :: Pool SqlBackend -> SqlPersistT IO a -> IO a
testDB = flip DB.runSqlPool
