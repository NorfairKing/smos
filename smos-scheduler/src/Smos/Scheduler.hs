{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Scheduler
  ( smosScheduler,
  )
where

import Control.Monad
import Control.Monad.Reader
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import Data.Time
import Data.Yaml as Yaml
import Path
import Path.IO
import Smos.Data
import qualified Smos.Report.Config as Report
import Smos.Scheduler.OptParse
import Smos.Scheduler.Render
import System.Cron (nextMatch, scheduleMatches)
import System.Exit
import Text.Show.Pretty

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
      mapM_ (handleScheduleItem mState wd now) $ scheduleItems setSchedule
      let state' =
            case mState of
              Nothing -> ScheduleState {scheduleStateLastRun = zonedTimeToUTC now}
              Just state -> state {scheduleStateLastRun = zonedTimeToUTC now}
      SB.writeFile (fromAbsFile setStateFile) (Yaml.encode state')
    else putStrLn "Not running because it's been run too recently already."

minimumScheduleInterval :: NominalDiffTime
minimumScheduleInterval = 60 -- Only run once per minute.

handleScheduleItem :: Maybe ScheduleState -> Path Abs Dir -> ZonedTime -> ScheduleItem -> IO ()
handleScheduleItem mState wdir now se = do
  let s = scheduleItemCronSchedule se
  let mScheduledTime =
        case mState of
          Nothing ->
            if scheduleMatches s (zonedTimeToUTC now)
              then Just (zonedTimeToUTC now)
              else Nothing
          Just ScheduleState {..} ->
            case nextMatch s scheduleStateLastRun of
              Nothing -> Nothing
              Just scheduled ->
                if scheduleStateLastRun <= scheduled && scheduled <= zonedTimeToUTC now
                  then Just scheduled
                  else Nothing
  case mScheduledTime of
    Nothing -> putStrLn $ unwords ["Not activating", show s, "at current time", show now]
    Just scheduledTime -> performScheduleItem wdir (utcToZonedTime (zonedTimeZone now) scheduledTime) se

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
      pPrint from
      pPrint to
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
              SB8.putStrLn cts
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
                    $ die
                    $ unwords ["ERROR: destination already exists:", fromAbsFile to, " not overwriting."]
                  ensureDir $ parent to
                  writeSmosFile to rendered
                  cs <- SB.readFile $ fromAbsFile to
                  SB8.putStrLn cs
