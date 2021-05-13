{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.Looper.BackupGarbageCollector where

import Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T
import Smos.Server.Backup
import Smos.Server.Looper.Import

runBackupGarbageCollectorLooper :: Looper ()
runBackupGarbageCollectorLooper = do
  mMaxBackups <- asks looperEnvMaxBackupsPerUser
  forM_ mMaxBackups $ \maxBackups -> do
    acqUserIdSource <- looperDB $ selectKeysRes [] [Asc UserId]
    withAcquire acqUserIdSource $ \source ->
      runConduit $ source .| C.mapM_ (backupGarbageCollectorForUser maxBackups)

backupGarbageCollectorForUser :: Word -> UserId -> Looper ()
backupGarbageCollectorForUser maxBackups uid = do
  logDebugNS "backup-garbage-collector" $ "Checking for garbage collection of backups for user " <> T.pack (show (fromSqlKey uid))
  currentBackups <- looperDB $ count [BackupUser ==. uid]
  let backupsToDelete = max 0 $ currentBackups - fromIntegral maxBackups
  backupIds <-
    looperDB $
      selectKeysList
        [BackupUser ==. uid]
        [ Asc BackupTime, -- Delete oldest backups first
          LimitTo backupsToDelete
        ]
  logDebugNS "backup-garbage-collector" $ "About to delete " <> T.pack (show (length backupIds)) <> " for user " <> T.pack (show (fromSqlKey uid))

  forM_ backupIds $ \backupId -> do
    logInfoNS "backup-garbage-collector" $ "Deleting backup " <> T.pack (show (fromSqlKey backupId)) <> " for user " <> T.pack (show (fromSqlKey uid))
    looperDB $ deleteBackupById backupId
