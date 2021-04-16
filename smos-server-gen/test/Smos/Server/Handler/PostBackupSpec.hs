module Smos.Server.Handler.PostBackupSpec
  ( spec,
  )
where

import Smos.Client
import Smos.Data.Gen ()
import Smos.Server.InterestingStore
import Smos.Server.TestUtils
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec =
  describe "PostBackup" $ do
    serverSpec $ do
      it "produces a valid smos directory forest for any interesting store" $ \cenv ->
        forAllValid $ \store ->
          withNewUser cenv $ \t -> testClient cenv $ do
            setupInterestingStore t store
            backupUuid <- clientPostBackup t
            liftIO $ shouldBeValid backupUuid
      it "doesn't change the stored files" $ \cenv ->
        forAllValid $ \store ->
          withNewUser cenv $ \t -> testClient cenv $ do
            setupInterestingStore t store
            filesBefore <- clientGetListSmosFiles t
            _ <- clientPostBackup t
            filesAfter <- clientGetListSmosFiles t
            liftIO $ filesAfter `shouldBe` filesBefore
