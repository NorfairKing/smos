{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Server.Looper.BackupGarbageCollector where

import Conduit
import qualified Data.Conduit.Combinators as C
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Database.Esqueleto.Experimental as E
import Database.Persist as DB
import Database.Persist.Pagination
import Smos.Server.Looper.Import

runBackupGarbageCollectorLooper :: Looper ()
runBackupGarbageCollectorLooper = do
  maxBackupsPerPeriod <- asks looperEnvMaxBackupsPerPeriodPerUser
  runConduit $
    looperDBConduit (streamEntities [] UserId (PageSize 16) Ascend (Range Nothing Nothing))
      .| C.map entityKey
      .| C.mapM_ (backupGarbageCollectorForUser maxBackupsPerPeriod)

  collectOrphanedBackupFiles

backupGarbageCollectorForUser :: [(NominalDiffTime, Word)] -> UserId -> Looper ()
backupGarbageCollectorForUser periods uid = do
  logDebugNS "backup-garbage-collector" $ "Checking for garbage collection of backups for user " <> T.pack (show (fromSqlKey uid))
  backups <- fmap (map (\(E.Value i, E.Value t) -> (i, t))) $
    looperDB $
      E.select $ do
        backup <- E.from $ E.table @Backup
        E.where_ $ backup E.^. BackupUser E.==. E.val uid
        pure (backup E.^. BackupId, backup E.^. BackupTime)
  now <- liftIO getCurrentTime
  let backupsToDelete = decideBackupsToDelete now periods backups

  logDebugNS "backup-garbage-collector" $ "About to delete " <> T.pack (show (length backupsToDelete)) <> " backups for user " <> T.pack (show (fromSqlKey uid))
  forM_ backupsToDelete $ \backupId -> do
    logInfoNS "backup-garbage-collector" $ "Deleting backup " <> T.pack (show (fromSqlKey backupId)) <> " for user " <> T.pack (show (fromSqlKey uid))
    looperDB $ DB.delete backupId

defaultPeriods :: [(NominalDiffTime, Word)]
defaultPeriods =
  [ (4 * 3600, 4), -- Every backup of the last four hours
    (nominalDay, 4), -- 4 of the last day
    (7 * nominalDay, 7), -- 7 of the last week
    (8 * 7 * nominalDay, 8), -- 8 of the last 2 months
    (365 * nominalDay, 12) -- 12 of the last year
  ]

decideBackupsToDelete :: UTCTime -> [(NominalDiffTime, Word)] -> [(BackupId, UTCTime)] -> Set BackupId
decideBackupsToDelete now periods backups =
  S.fromList (map fst backups) `S.difference` decideBackupsToKeep now periods backups

decideBackupsToKeep :: UTCTime -> [(NominalDiffTime, Word)] -> [(BackupId, UTCTime)] -> Set BackupId
decideBackupsToKeep now bigPeriods backups =
  S.unions $
    flip map bigPeriods $ \(bigPeriod, maxBackupsInThisPeriod) ->
      let smallPeriodSize = bigPeriod / fromIntegral maxBackupsInThisPeriod
          smallPeriods =
            map
              (\i -> addUTCTime (-(fromIntegral i * smallPeriodSize)) now)
              [1 .. maxBackupsInThisPeriod]
       in S.unions $
            flip map smallPeriods $ \smallPeriodBegin ->
              let begin = smallPeriodBegin
                  end = now
                  timeInPeriod :: UTCTime -> Bool
                  timeInPeriod t = begin < t && t <= end
                  backupsInThisPeriod = map fst $ sortOn snd $ filter (timeInPeriod . snd) backups
               in S.fromList $ take 1 backupsInThisPeriod

collectOrphanedBackupFiles :: Looper ()
collectOrphanedBackupFiles = do
  logDebugNS "backup-garbage-collector" "Collecting orphaned backup files"
  looperDB $
    E.delete $ do
      casFile <- E.from $ E.table @CasFile
      E.where_ $ E.notExists $ do
        backupFile <- E.from $ E.table @BackupFile
        E.where_ $ backupFile E.^. BackupFileFile E.==. casFile E.^. CasFileId
