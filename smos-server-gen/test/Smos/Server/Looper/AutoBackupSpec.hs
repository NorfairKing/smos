module Smos.Server.Looper.AutoBackupSpec where

import Data.Time
import Data.UUID.Typed
import Database.Persist.Sql
import Smos.API
import Smos.Server.DB
import Smos.Server.Looper.AutoBackup
import Smos.Server.TestUtils
import Test.Syd

spec :: Spec
spec = serverEnvSpec $ do
  describe "runAutoBackupLooper" $ do
    it "makes a backup for one user if none have been made yet." $ \env -> runServerTestEnvM env $ do
      withNewRegisteredUser (serverTestEnvClientEnv env) $ \register -> do
        mUser <- serverEnvDB $ selectFirst [UserName ==. registerUsername register] [Asc UserId]
        case mUser of
          Nothing -> liftIO $ expectationFailure "expected a user."
          Just (Entity uid _) -> do
            serverEnvLooper runAutoBackupLooper
            countAfterwards <- serverEnvDB $ count [BackupUser ==. uid]
            liftIO $ countAfterwards `shouldBe` 1

    it "makes another backup if one has been made already but long ago enough" $ \env -> runServerTestEnvM env $ do
      withNewRegisteredUser (serverTestEnvClientEnv env) $ \register -> do
        mUser <- serverEnvDB $ selectFirst [UserName ==. registerUsername register] [Asc UserId]
        case mUser of
          Nothing -> liftIO $ expectationFailure "expected a user."
          Just (Entity uid _) -> do
            uuid <- nextRandomUUID
            now <- liftIO getCurrentTime
            let twoDaysAgo = addUTCTime (-(2 * nominalDay)) now
            -- Make a backup that was supposedly done two days ago
            serverEnvDB $
              insert_
                Backup
                  { backupUuid = uuid,
                    backupUser = uid,
                    backupSize = 0,
                    backupTime = twoDaysAgo
                  }
            -- The user has been used since that backup.
            serverEnvDB $ update uid [UserLastUse =. Just now]
            -- Run the looper again
            serverEnvLooper runAutoBackupLooper
            -- Should have two in total now.
            countAfterwards <- serverEnvDB $ count [BackupUser ==. uid]
            liftIO $ countAfterwards `shouldBe` 2
