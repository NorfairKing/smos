{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Scheduler.Recurrence
  ( computeLastRun,
    computeNextRun,
    computeScheduledTime,
    ScheduledTime (..),
    scheduleHashPropertyName,
  )
where

import Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Map as M
import Data.Maybe
import Data.Time
import Data.Tree
import GHC.Generics (Generic)
import Safe
import Smos.Data
import Smos.Report.Archive
import Smos.Report.Config
import Smos.Report.ShouldPrint
import Smos.Report.Streaming
import Smos.Report.Time (timeNominalDiffTime)
import Smos.Scheduler.OptParse.Types
import System.Cron as Cron

computeLastRun :: DirectoryConfig -> ScheduleItem -> IO (Maybe UTCTime)
computeLastRun dc si = do
  workflowDir <- resolveDirWorkflowDir dc

  runConduit $
    streamSmosFilesFromWorkflowRel Don'tHideArchive dc
      .| parseSmosFilesRel workflowDir
      .| printShouldPrint DontPrint -- TODO make this configurable
      .| C.map snd
      .| C.filter (fileMatchesSchedule (hashScheduleItem si))
      .| C.concatMap earliestStateChangeInSmosFile
      .| C.maximum

computeNextRun :: DirectoryConfig -> UTCTime -> ScheduleItem -> IO (Maybe UTCTime)
computeNextRun dc now si = case scheduleItemRecurrence si of
  RentRecurrence cs -> do
    mLastRun <- computeLastRun dc si
    pure $ Cron.nextMatch cs (fromMaybe now mLastRun)
  HaircutRecurrence t -> haircutNextRun dc now (timeNominalDiffTime t) (hashScheduleItem si)

haircutNextRun :: DirectoryConfig -> UTCTime -> NominalDiffTime -> ScheduleItemHash -> IO (Maybe UTCTime)
haircutNextRun dc now ndt h = do
  workflowDir <- resolveDirWorkflowDir dc

  let lastMatchingFileIn ha =
        runConduit $
          streamSmosFilesFromWorkflowRel ha dc
            .| parseSmosFilesRel workflowDir
            .| printShouldPrint DontPrint -- TODO make this configurable
            .| C.map snd
            .| C.filter (fileMatchesSchedule h)
            .| C.concatMap latestStateChangeInSmosFile
            .| C.maximum

  mLastRunInWorkflow <- lastMatchingFileIn HideArchive
  case mLastRunInWorkflow of
    Just _ -> pure Nothing -- Still in flight, don't reactivate
    Nothing ->
      Just <$> do
        mLastRunInArchive <- lastMatchingFileIn Don'tHideArchive
        pure $ case mLastRunInArchive of
          Nothing -> now
          Just lastRunInArchive -> addUTCTime ndt lastRunInArchive

fileMatchesSchedule :: ScheduleItemHash -> SmosFile -> Bool
fileMatchesSchedule h sf = case smosFileForest sf of
  [] -> False
  (Node e _ : _) -> entryMatchesSchedule h e

earliestStateChangeInSmosFile :: SmosFile -> Maybe UTCTime
earliestStateChangeInSmosFile = minimumMay . smosFileStateChanges

latestStateChangeInSmosFile :: SmosFile -> Maybe UTCTime
latestStateChangeInSmosFile = maximumMay . smosFileStateChanges

smosFileStateChanges :: SmosFile -> [UTCTime]
smosFileStateChanges = concatMap entryStateChanges . concatMap flatten . smosFileForest

entryStateChanges :: Entry -> [UTCTime]
entryStateChanges = stateHistoryStateChanges . entryStateHistory

stateHistoryStateChanges :: StateHistory -> [UTCTime]
stateHistoryStateChanges = map stateHistoryEntryTimestamp . unStateHistory

entryMatchesSchedule :: ScheduleItemHash -> Entry -> Bool
entryMatchesSchedule h e = case M.lookup scheduleHashPropertyName (entryProperties e) of
  Nothing -> False
  Just hashProperty -> propertyValue (renderScheduleItemHash h) == Just hashProperty

scheduleHashPropertyName :: PropertyName
scheduleHashPropertyName = "schedule-hash"

computeScheduledTime :: DirectoryConfig -> UTCTime -> ScheduleItem -> IO ScheduledTime
computeScheduledTime dc now si = do
  mLastRun <- computeLastRun dc si
  case mLastRun of
    Nothing -> pure $ ActivateAt now
    Just lastRun -> do
      mNextRun <- computeNextRun dc now si
      pure $ case mNextRun of
        Nothing -> Don'tActivate
        Just nextRun ->
          if lastRun <= nextRun && nextRun <= now
            then ActivateAt nextRun
            else Don'tActivate

data ScheduledTime
  = ActivateAt UTCTime
  | Don'tActivate
  deriving (Show, Eq, Generic)
