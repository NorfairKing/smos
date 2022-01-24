{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Server.Looper.BackupGarbageCollector where

import Conduit
import qualified Data.Conduit.Combinators as C
import Data.List
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Smos.Server.Backup
import Smos.Server.Looper.Import

runBackupGarbageCollectorLooper :: Looper ()
runBackupGarbageCollectorLooper = do
  maxBackupsPerPeriod <- asks looperEnvMaxBackupsPerPeriodPerUser
  acqUserIdSource <- looperDB $ selectKeysRes [] [Asc UserId]
  withAcquire acqUserIdSource $ \source ->
    runConduit $ source .| C.mapM_ (backupGarbageCollectorForUser maxBackupsPerPeriod)

backupGarbageCollectorForUser :: [(Maybe NominalDiffTime, Word)] -> UserId -> Looper ()
backupGarbageCollectorForUser periods uid = do
  logDebugNS "backup-garbage-collector" $ "Checking for garbage collection of backups for user " <> T.pack (show (fromSqlKey uid))
  backups <-
    looperDB $
      selectList
        [BackupUser ==. uid]
        [Desc BackupTime] -- Delete oldest backups first
        -- TODO use a more specific query instead of 'map' ing
  now <- liftIO getCurrentTime
  let backupsToDelete =
        decideBackupsToDelete now periods $
          map (\(Entity backupId Backup {..}) -> (backupId, backupTime)) backups
  logDebugNS "backup-garbage-collector" $ "About to delete " <> T.pack (show (length backupsToDelete)) <> " backups for user " <> T.pack (show (fromSqlKey uid))
  forM_ backupsToDelete $ \backupId -> do
    logInfoNS "backup-garbage-collector" $ "Deleting backup " <> T.pack (show (fromSqlKey backupId)) <> " for user " <> T.pack (show (fromSqlKey uid))
    looperDB $ deleteBackupById backupId

defaultPeriods :: [(Maybe NominalDiffTime, Word)]
defaultPeriods =
  [ (Just $ 7 * nominalDay, 7),
    (Just $ 8 * 7 * nominalDay, 8),
    (Just $ 365 * nominalDay, 12)
  ]

decideBackupsToDelete :: UTCTime -> [(Maybe NominalDiffTime, Word)] -> [(BackupId, UTCTime)] -> Set BackupId
decideBackupsToDelete now periods backups =
  S.fromList (map fst backups) `S.difference` decideBackupsToKeep now periods backups

decideBackupsToKeep :: UTCTime -> [(Maybe NominalDiffTime, Word)] -> [(BackupId, UTCTime)] -> Set BackupId
decideBackupsToKeep now bigPeriods backups =
  S.unions $
    flip map bigPeriods $ \(mBigPeriod, maxBackupsInThisPeriod) ->
      case mBigPeriod of
        Nothing ->
          S.fromList $
            take (fromIntegral maxBackupsInThisPeriod) $ map fst $ sortOn (Down . snd) backups
        Just bigPeriod ->
          let smallPeriodSize = bigPeriod / fromIntegral maxBackupsInThisPeriod
              smallPeriods =
                map
                  (\i -> addUTCTime (- fromIntegral i * smallPeriodSize) now)
                  [1 .. maxBackupsInThisPeriod]
           in S.unions $
                flip map smallPeriods $ \smallPeriodBegin ->
                  let begin = smallPeriodBegin
                      end = now
                      timeInPeriod :: UTCTime -> Bool
                      timeInPeriod t = begin < t && t <= end
                      backupsInThisPeriod = map fst $ sortOn snd $ filter (timeInPeriod . snd) backups
                   in S.fromList $ take 1 backupsInThisPeriod
