{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Scheduler
  ( smosScheduler,
  )
where

import Control.Monad
import Control.Monad.Reader
import qualified Data.ByteString as SB
import qualified Data.Map as M
import Data.Time
import Data.Yaml as Yaml
import Path
import Path.IO
import Smos.Data
import qualified Smos.Report.Config as Report
import Smos.Scheduler.OptParse
import Smos.Scheduler.Render
import System.Cron (CronSchedule, nextMatch, scheduleMatches)
import System.Exit

smosScheduler :: IO ()
smosScheduler = getSettings >>= scheduler

scheduler :: Settings -> IO ()
scheduler Settings {..} = do
  wd <- Report.resolveDirWorkflowDir setDirectorySettings
  mContents <- forgivingAbsence $ SB.readFile $ fromAbsFile setStateFile
  mState <-
    case mContents of
      Nothing -> pure Nothing
      Just cts ->
        case Yaml.decodeEither' cts of
          Left err ->
            die $
              unlines
                [ unwords ["ERROR: unable to decode state file:", fromAbsFile setStateFile],
                  prettyPrintParseException err
                ]
          Right state -> pure $ Just state
  now <- getZonedTime
  let goAhead =
        case mState of
          Nothing -> True
          Just ScheduleState {..} -> diffUTCTime (zonedTimeToUTC now) scheduleStateLastRun >= minimumScheduleInterval
  if goAhead
    then do
      state' <- handleSchedule mState wd now setSchedule
      SB.writeFile (fromAbsFile setStateFile) (Yaml.encode state')
    else putStrLn "Not running because it's been run too recently already."

minimumScheduleInterval :: NominalDiffTime
minimumScheduleInterval = 60 -- Only run once per minute.

handleSchedule :: Maybe ScheduleState -> Path Abs Dir -> ZonedTime -> Schedule -> IO ScheduleState
handleSchedule mState wd now schedule = do
  let startingState = case mState of
        Nothing -> ScheduleState {scheduleStateLastRun = zonedTimeToUTC now, scheduleStateLastRuns = M.empty}
        Just s -> s {scheduleStateLastRun = zonedTimeToUTC now}
  let go :: ScheduleState -> ScheduleItem -> IO ScheduleState
      go s si = do
        let ms = M.lookup (hashScheduleItem si) (scheduleStateLastRuns s)
        mu <- handleScheduleItem ms wd now si
        case mu of
          Nothing -> pure s
          Just newLastRun -> pure s {scheduleStateLastRuns = M.insert (hashScheduleItem si) newLastRun (scheduleStateLastRuns s)}
  foldM go startingState (scheduleItems schedule)

handleScheduleItem :: Maybe UTCTime -> Path Abs Dir -> ZonedTime -> ScheduleItem -> IO (Maybe UTCTime)
handleScheduleItem mLastRun wdir now se = do
  let s = scheduleItemCronSchedule se
  let mScheduledTime = calculateScheduledTime (zonedTimeToUTC now) mLastRun s
  case mScheduledTime of
    Nothing -> do
      putStrLn $ unwords ["Not activating", show s, "at current time", show now]
      pure Nothing
    Just scheduledTime -> do
      performScheduleItem wdir (utcToZonedTime (zonedTimeZone now) scheduledTime) se
      pure $ Just scheduledTime

calculateScheduledTime :: UTCTime -> Maybe UTCTime -> CronSchedule -> Maybe UTCTime
calculateScheduledTime now mState s =
  case mState of
    Nothing ->
      if scheduleMatches s now
        then Just now
        else Nothing
    Just lastRun ->
      case nextMatch s lastRun of
        Nothing -> Nothing
        Just scheduled ->
          if lastRun <= scheduled && scheduled <= now
            then Just scheduled
            else Nothing

performScheduleItem :: Path Abs Dir -> ZonedTime -> ScheduleItem -> IO ()
performScheduleItem wdir now ScheduleItem {..} = do
  let from = wdir </> scheduleItemTemplate
  let ctx = RenderContext {renderContextTime = now}
  case runReaderT (renderPathTemplate scheduleItemDestination) ctx of
    Failure errs ->
      putStrLn
        $ unlines
        $ "ERROR: Validation errors while rendering template destination file name:"
          : map prettyRenderError errs
    Success destination -> do
      let to = wdir </> destination
      mContents <- forgivingAbsence $ SB.readFile $ fromAbsFile from
      case mContents of
        Nothing -> putStrLn $ unwords ["ERROR: template does not exist:", fromAbsFile from]
        Just cts ->
          case Yaml.decodeEither' cts of
            Left err ->
              putStrLn $
                unlines
                  [ unwords ["ERROR: Does not look like a smos template file:", fromAbsFile from],
                    prettyPrintParseException err
                  ]
            Right template -> do
              let vRendered = runReaderT (renderTemplate template) ctx
              case vRendered of
                Failure errs ->
                  putStrLn
                    $ unlines
                    $ "ERROR: Validation errors while rendering template:"
                      : map prettyRenderError errs
                Success rendered -> do
                  destinationExists <- doesFileExist to
                  when destinationExists
                    $ putStrLn
                    $ unwords ["WARNING: destination already exists:", fromAbsFile to, " not overwriting."]
                  ensureDir $ parent to
                  writeSmosFile to rendered
