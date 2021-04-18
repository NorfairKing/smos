module Smos.Server.Handler.DeleteBackupSpec
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
  describe "DeleteBackup" $ do
    serverSpec $ do
      it "makes a backup un-get-able" $ \cenv ->
        forAllValid $ \store1 ->
          withNewUser cenv $ \t -> testClient cenv $ do
            setupInterestingStore t store1
            backupUuid <- clientPostBackup t
            NoContent <- clientDeleteBackup t backupUuid
            backups <- clientGetListBackups t
            liftIO $ backupUuid `shouldNotSatisfy` (`elem` map backupInfoUUID backups)
