{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Looper.AutoBackup (runAutoBackupLooper) where

import Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T
import Database.Persist.Pagination
import Smos.Server.Backup
import Smos.Server.Looper.Import

runAutoBackupLooper :: Looper ()
runAutoBackupLooper =
  runConduit $
    looperDBConduit (streamEntities [] UserId (PageSize 16) Ascend (Range Nothing Nothing))
      .| C.filterM checkIfElligible
      .| C.map entityKey
      .| C.mapM_ autoBackupForUser

checkIfElligible :: Entity User -> Looper Bool
checkIfElligible (Entity uid User {..}) = do
  mMostRecentBackup <- looperDB $ selectFirst [BackupUser ==. uid] [Desc BackupTime]
  let possiblyUpdatedSinceLastBackup = case mMostRecentBackup of
        Nothing -> True
        Just (Entity _ Backup {..}) -> case userLastUse of
          Nothing -> False
          Just lastUse -> backupTime < lastUse

  if possiblyUpdatedSinceLastBackup
    then pure True
    else do
      logDebugNS "auto-backup" $
        T.pack $
          unwords
            [ "Not performing backup for user",
              show (fromSqlKey uid),
              "because it hasn't been used since the last backup."
            ]
      pure False

autoBackupForUser :: UserId -> Looper ()
autoBackupForUser uid = do
  logInfoNS "auto-backup" $
    T.pack $
      unwords
        [ "Performing backup for user",
          show (fromSqlKey uid)
        ]
  compressionLevel <- asks looperEnvCompressionLevel
  void $ looperDB $ doBackupForUser compressionLevel uid
