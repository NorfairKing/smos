module Smos.Sync.Client.Sync.MetaSpec
  ( spec,
  )
where

import Data.Pool
import Database.Persist.Sqlite as DB
import Smos.Sync.Client.Meta
import Smos.Sync.Client.MetaMap.Gen ()
import Smos.Sync.Client.TestUtils
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  describe "makeClientMetaData"
    $ it "produces valid meta maps"
    $ producesValidsOnValids2 makeClientMetaData
  clientDBSpec
    $ describe "writeClientMetadata"
    $ do
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
