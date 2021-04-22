{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.Looper.BackupGarbageCollector where

import qualified Data.Text as T
import Smos.Server.Backup
import Smos.Server.Looper.Import

runBackupGarbageCollectorLooper :: Looper ()
runBackupGarbageCollectorLooper = do
  mMaxBackups <- asks looperEnvMaxBackupsPerUser
  forM_ mMaxBackups $ \maxBackups -> do
    -- TODO do this in a conduit so we don't load all users
    userIds <- looperDB $ selectKeysList [] [Asc UserId]
    mapM_ (backupGarbageCollectorForUser maxBackups) userIds

backupGarbageCollectorForUser :: Word -> UserId -> Looper ()
backupGarbageCollectorForUser maxBackups uid = do
  logDebugNS "auto-backup" $ "Checking for garbage collection of backups for user " <> T.pack (show (fromSqlKey uid))
  currentBackups <- looperDB $ count [BackupUser ==. uid]
  let backupsToDelete = max 0 $ currentBackups - fromIntegral maxBackups
  backupIds <-
    looperDB $
      selectKeysList
        [BackupUser ==. uid]
        [ Asc BackupTime, -- Delete oldest backups first
          LimitTo backupsToDelete
        ]
  mapM_ (looperDB . deleteBackupById) backupIds
