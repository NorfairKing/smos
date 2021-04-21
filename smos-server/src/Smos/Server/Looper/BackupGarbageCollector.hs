{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.Looper.BackupGarbageCollector where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Text as T
import Data.Time
import Database.Persist
import Smos.Server.Backup
import Smos.Server.DB
import Smos.Server.Looper.Env

runBackupGarbageCollectorLooper :: Looper ()
runBackupGarbageCollectorLooper = do
  -- TODO do this in a conduit so we don't load all users
  userIds <- looperDB $ selectKeysList [] [Asc UserId]
  mapM_ backupGarbageCollectorForUser userIds

backupGarbageCollectorForUser :: UserId -> Looper ()
backupGarbageCollectorForUser uid = do
  logDebugNS "auto-backup" $ "Checking for garbage collection of backups for user " <> T.pack (show uid)
  pure ()
