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
  describe "runAutoBackupLooper" $
    it "makes a backup for one user if none have been made yet." $ \env -> runServerTestEnvM env $ do
      withNewRegisteredUser (serverTestEnvClientEnv env) $ \register -> do
        mUser <- serverEnvDB $ selectFirst [UserName ==. registerUsername register] [Asc UserId]
        case mUser of
          Nothing -> liftIO $ expectationFailure "expected a user."
          Just (Entity uid _) -> do
            serverEnvLooper runAutoBackupLooper
            countAfterwards <- serverEnvDB $ count [BackupUser ==. uid]
            liftIO $ countAfterwards `shouldBe` 1
  describe "autoBackupForUser" $ do
    it "makes no backup if one has been made too recently already" $ \env -> runServerTestEnvM env $ do
      withNewRegisteredUser (serverTestEnvClientEnv env) $ \register -> do
        mUser <- serverEnvDB $ selectFirst [UserName ==. registerUsername register] [Asc UserId]
        case mUser of
          Nothing -> liftIO $ expectationFailure "expected a user."
          Just (Entity uid _) -> do
            serverEnvLooper (autoBackupForUser uid) -- A backup will be made the first time
            serverEnvLooper (autoBackupForUser uid) -- but not the second
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
            let twoDaysAgo = addUTCTime (- 2 * nominalDay) now
            -- Make a backup that was supposedly done two days ago
            serverEnvDB $
              insert_
                Backup
                  { backupUuid = uuid,
                    backupUser = uid,
                    backupSize = 0,
                    backupTime = twoDaysAgo
                  }
            serverEnvLooper (autoBackupForUser uid) -- a second one should be made
            countAfterwards <- serverEnvDB $ count [BackupUser ==. uid]
            liftIO $ countAfterwards `shouldBe` 2
