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
import Data.Hashable
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Data.Tree
import GHC.Generics (Generic)
import Safe
import Smos.Data
import Smos.Report.Archive
import Smos.Report.Config
import Smos.Report.ShouldPrint
import Smos.Report.Streaming
import Smos.Report.Stuck
import Smos.Report.Time (timeNominalDiffTime)
import Smos.Scheduler.OptParse.Types
import System.Cron as Cron

computeLastRun :: Maybe ScheduleState -> ScheduleItem -> Maybe UTCTime
computeLastRun mState si =
  mState >>= \s ->
    M.lookup (hashScheduleItem si) (scheduleStateLastRuns s)

computeNextRun :: DirectoryConfig -> Maybe ScheduleState -> UTCTime -> ScheduleItem -> IO (Maybe UTCTime)
computeNextRun dc mState now si = case scheduleItemRecurrence si of
  RentRecurrence cs -> do
    let mLastRun = computeLastRun mState si
    pure $ rentNextRun cs (fromMaybe now mLastRun)
  HaircutRecurrence t -> haircutNextRun dc now (timeNominalDiffTime t) (hash si)

rentNextRun :: CronSchedule -> UTCTime -> Maybe UTCTime
rentNextRun = Cron.nextMatch

haircutNextRun :: DirectoryConfig -> UTCTime -> NominalDiffTime -> Int -> IO (Maybe UTCTime)
haircutNextRun dc now ndt h = do
  workflowDir <- resolveDirWorkflowDir dc
  projectsDir <- resolveDirProjectsDir dc
  archiveDir <- resolveDirArchiveDir dc

  let lastMatchingFileIn ha =
        runConduit $
          streamSmosFilesFromWorkflowRel ha dc
            .| parseSmosFilesRel workflowDir
            .| printShouldPrint DontPrint -- TODO make this configurable
            .| C.map snd
            .| C.concatMap (fileMatchesSchedule h)
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

fileMatchesSchedule :: Int -> SmosFile -> Maybe UTCTime
fileMatchesSchedule h sf = case smosFileForest sf of
  [] -> Nothing
  (Node e _ : _) ->
    if entryMatchesSchedule h e
      then -- Note: No state change means the file doesn't match.

        latestStateChangeInSmosFile sf
      else Nothing

latestStateChangeInSmosFile :: SmosFile -> Maybe UTCTime
latestStateChangeInSmosFile sf =
  maximumMay $
    mapMaybe
      (latestStateChange . entryStateHistory)
      (concatMap flatten (smosFileForest sf))

entryMatchesSchedule :: Int -> Entry -> Bool
entryMatchesSchedule h e = case M.lookup scheduleHashPropertyName (entryProperties e) of
  Nothing -> False
  Just hashProperty -> propertyValue (T.pack (show h)) == Just hashProperty

scheduleHashPropertyName :: PropertyName
scheduleHashPropertyName = "schedule-hash"

computeScheduledTime :: DirectoryConfig -> Maybe ScheduleState -> UTCTime -> ScheduleItem -> IO ScheduledTime
computeScheduledTime dc mState now si =
  case computeLastRun mState si of
    Nothing -> pure $ Don'tActivateButUpdate now
    Just lastRun -> do
      mNextRun <- computeNextRun dc mState now si
      pure $ case mNextRun of
        Nothing -> Don'tActivate
        Just nextRun ->
          if lastRun <= nextRun && nextRun <= now
            then ActivateAt nextRun
            else Don'tActivate

data ScheduledTime
  = ActivateAt UTCTime
  | Don'tActivate
  | Don'tActivateButUpdate UTCTime
  deriving (Show, Eq, Generic)
