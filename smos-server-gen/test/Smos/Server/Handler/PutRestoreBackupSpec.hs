module Smos.Server.Handler.PutRestoreBackupSpec
  ( spec,
  )
where

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
    serverSpec $
      it "produces a valid smos directory forest for any interesting store" $ \cenv ->
        forAllValid $ \store1 ->
          forAll (genValid `suchThat` (/= store1)) $ \store2 ->
            withNewUser cenv $ \t -> testClient cenv $ do
              setupInterestingStore t store1
              beforeBackup <- clientGetListSmosFiles t
              backupUuid <- clientPostBackup t
              setupInterestingStore t store2
              afterBackup <- clientGetListSmosFiles t
              liftIO $ afterBackup `shouldNotBe` beforeBackup
              NoContent <- clientPutRestoreBackup t backupUuid
              afterBackupRestore <- clientGetListSmosFiles t
              liftIO $ afterBackupRestore `shouldBe` beforeBackup
