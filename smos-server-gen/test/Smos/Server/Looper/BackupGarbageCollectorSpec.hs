module Smos.Server.Looper.BackupGarbageCollectorSpec where

import Control.Monad
import Database.Persist.Sql
import Smos.API
import Smos.Server.Backup
import Smos.Server.DB
import Smos.Server.Env
import Smos.Server.Looper.BackupGarbageCollector
import Smos.Server.TestUtils
import Test.Syd

spec :: Spec
spec = serverEnvSpec $ do
  describe "backupGarbageCollectorForUser" $
    it "leaves the maximum number of backups" $ \env -> runServerTestEnvM env $ do
      withNewRegisteredUser (serverTestEnvClientEnv env) $ \register -> do
        mUser <- serverEnvDB $ selectFirst [UserName ==. registerUsername register] [Asc UserId]
        case mUser of
          Nothing -> liftIO $ expectationFailure "expected a user."
          Just (Entity uid _) -> do
            let numberOfBackupsToMake = 5
            let maxBackupsPerUser = 2
            replicateM_ numberOfBackupsToMake $ serverEnvDB $ doBackupForUser (serverEnvCompressionLevel (serverTestEnvServerEnv env)) uid
            serverEnvLooper $ backupGarbageCollectorForUser maxBackupsPerUser uid
            countAfterwards <- serverEnvDB $ count [BackupUser ==. uid]
            liftIO $ countAfterwards `shouldBe` fromIntegral maxBackupsPerUser -- Safe because maxBackups is small.
