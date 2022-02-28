{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Handler.PutRestoreBackupSpec
  ( spec,
  )
where

import qualified Data.Mergeful as Mergeful
import Smos.Client
import Smos.Data.Gen ()
import Smos.Server.InterestingStore
import Smos.Server.TestUtils
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec =
  describe "PutRestoreBackup" $ do
    serverSpec $ do
      it "produces a valid smos directory forest for any interesting store" $ \cenv ->
        forAllValid $ \store1 ->
          forAll (genValid `suchThat` (/= store1)) $ \store2 ->
            withNewUser cenv $ \t -> testClient cenv $ do
              setupInterestingStore t store1
              beforeBackup <- clientGetListSmosFiles t
              backupUuid <- clientPostBackup t
              setupInterestingStore t store2
              NoContent <- clientPutRestoreBackup t backupUuid
              afterBackupRestore <- clientGetListSmosFiles t
              liftIO $ afterBackupRestore `shouldBe` beforeBackup
      -- This test ensures that restoring backups deals with server times correctly
      it "it can reset the state to what there was at the time of the backup after restoring a backup" $ \cenv ->
        forAllValid $ \clientStoreBefore ->
          forAllValid $ \clientStoreAfter ->
            withNewUser cenv $ \t -> testClient cenv $ do
              let merge cstore SyncResponse {..} = Mergeful.mergeSyncResponseFromServer cstore syncResponseItems
              let req1 = SyncRequest {syncRequestItems = Mergeful.makeSyncRequest clientStoreBefore}
              resp1 <- clientPostSync t req1
              let clientStoreRightBefore = merge clientStoreBefore resp1
              backupUuid <- clientPostBackup t
              let req2 = SyncRequest {syncRequestItems = Mergeful.makeSyncRequest clientStoreAfter}
              resp2 <- clientPostSync t req2
              let clientStoreRightAfter = merge clientStoreAfter resp2
              NoContent <- clientPutRestoreBackup t backupUuid
              let req3 = SyncRequest {syncRequestItems = Mergeful.makeSyncRequest clientStoreRightAfter}
              resp3 <- clientPostSync t req3
              let clientStoreAfterAll = merge clientStoreRightAfter resp3
              -- The server times should be different so we cannot just compare the stores directly
              liftIO $ Mergeful.clientStoreItems clientStoreAfterAll `shouldBe` Mergeful.clientStoreItems clientStoreRightBefore
