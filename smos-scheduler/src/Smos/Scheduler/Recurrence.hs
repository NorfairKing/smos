{-# LANGUAGE DeriveGeneric #-}

module Smos.Scheduler.Recurrence
  ( computeLastRun,
    computeNextRun,
    computeScheduledTime,
    ScheduledTime (..),
  )
where

import qualified Data.Map as M
import Data.Maybe
import Data.Time
import GHC.Generics (Generic)
import Smos.Scheduler.OptParse.Types
import System.Cron as Cron

computeLastRun :: Maybe ScheduleState -> ScheduleItem -> Maybe UTCTime
computeLastRun mState si =
  mState >>= \s ->
    M.lookup (hashScheduleItem si) (scheduleStateLastRuns s)

computeNextRun :: UTCTime -> Maybe ScheduleState -> ScheduleItem -> IO (Maybe UTCTime)
computeNextRun now mState si = case scheduleItemRecurrence si of
  RentRecurrence cs -> do
    let mLastRun = computeLastRun mState si
    pure $ rentNextRun cs (fromMaybe now mLastRun)
  HaircutRecurrence ndt -> undefined

rentNextRun :: CronSchedule -> UTCTime -> Maybe UTCTime
rentNextRun = Cron.nextMatch

haircutNextRun :: NominalDiffTime -> IO (Maybe UTCTime)
haircutNextRun = undefined

computeScheduledTime :: UTCTime -> Maybe ScheduleState -> ScheduleItem -> IO ScheduledTime
computeScheduledTime now mState si =
  case computeLastRun mState si of
    Nothing -> pure $ Don'tActivateButUpdate now
    Just lastRun -> do
      mNextRun <- computeNextRun now mState si
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
