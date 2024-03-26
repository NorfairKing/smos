{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.Looper.AutoBackup where

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
      .| C.map entityKey
      .| C.mapM_ autoBackupForUser

autoBackupForUser :: UserId -> Looper ()
autoBackupForUser uid = do
  logInfoNS "auto-backup" $ T.pack $ unwords ["Performing backup for user", show (fromSqlKey uid)]
  compressionLevel <- asks looperEnvCompressionLevel
  void $ looperDB $ doBackupForUser compressionLevel uid
