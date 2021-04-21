{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.Looper.BackupGarbageCollector where

import qualified Data.Text as T
import Smos.Server.Looper.Import

runBackupGarbageCollectorLooper :: Looper ()
runBackupGarbageCollectorLooper = do
  -- TODO do this in a conduit so we don't load all users
  userIds <- looperDB $ selectKeysList [] [Asc UserId]
  mapM_ backupGarbageCollectorForUser userIds

backupGarbageCollectorForUser :: UserId -> Looper ()
backupGarbageCollectorForUser uid = do
  logDebugNS "auto-backup" $ "Checking for garbage collection of backups for user " <> T.pack (show (fromSqlKey uid))
  pure ()
