module Smos.Server.Handler.GetListBackupsSpec
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
  describe "GetListBackups" $ do
    serverSpec $
      it "correctly lists two backups after making two backups" $ \cenv ->
        forAllValid $ \store1 ->
          forAll (genValid `suchThat` (/= store1)) $ \store2 ->
            withNewUser cenv $ \t -> testClient cenv $ do
              setupInterestingStore t store1
              _ <- clientPostBackup t
              setupInterestingStore t store2
              _ <- clientPostBackup t
              bs <- clientGetListBackups t
              liftIO $ length bs `shouldBe` 2
