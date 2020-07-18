{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Scheduler.Commands.Schedule
  ( schedule,
  )
where

import Control.Monad
import Control.Monad.Reader
import qualified Data.ByteString as SB
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time
import Data.Yaml as Yaml
import GHC.Generics (Generic)
import Path
import Path.IO
import Smos.Data
import qualified Smos.Report.Config as Report
import Smos.Scheduler.OptParse
import Smos.Scheduler.Render
import Smos.Scheduler.Utils
import System.Cron (CronSchedule, nextMatch, scheduleMatches)

schedule :: Settings -> IO ()
schedule Settings {..} = do
  wd <- Report.resolveDirWorkflowDir setDirectorySettings
  mState <- readStateFile setStateFile
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

handleSchedule :: Maybe ScheduleState -> Path Abs Dir -> ZonedTime -> Schedule -> IO ScheduleState
handleSchedule mState wd now sched = do
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
  foldM go startingState (scheduleItems sched)

handleScheduleItem :: Maybe UTCTime -> Path Abs Dir -> ZonedTime -> ScheduleItem -> IO (Maybe UTCTime)
handleScheduleItem mLastRun wdir now si = do
  let s = scheduleItemCronSchedule si
  let mScheduledTime = calculateScheduledTime (zonedTimeToUTC now) mLastRun s
  case mScheduledTime of
    Don'tActivateButUpdate newLastRun -> do
      putStrLn $ unwords ["Not activating", T.unpack (scheduleItemDisplayName si), "but still setting its last run because it's the first time"]
      pure (Just newLastRun)
    Don'tActivate -> do
      putStrLn $ unwords ["Not activating", T.unpack (scheduleItemDisplayName si)]
      pure Nothing
    ActivateAt scheduledTime -> do
      r <- performScheduleItem wdir (utcToZonedTime (zonedTimeZone now) scheduledTime) si
      case scheduleItemResultMessage r of
        Nothing -> do
          putStrLn $ "Succesfully activated " <> T.unpack (scheduleItemDisplayName si)
          pure $ Just scheduledTime
        Just msg -> do
          putStrLn msg
          pure Nothing

scheduleItemDisplayName :: ScheduleItem -> Text
scheduleItemDisplayName si@ScheduleItem {..} =
  fromMaybe
    (T.unwords ["the item with schedule", T.pack (show scheduleItemCronSchedule), "and hash", T.pack (show (hashScheduleItem si))])
    scheduleItemDescription

calculateScheduledTime :: UTCTime -> Maybe UTCTime -> CronSchedule -> ScheduledTime
calculateScheduledTime now mState s =
  case mState of
    Nothing ->
      if scheduleMatches s now
        then ActivateAt now
        else Don'tActivateButUpdate now
    Just lastRun ->
      case nextMatch s lastRun of
        Nothing -> Don'tActivate
        Just scheduled ->
          if lastRun <= scheduled && scheduled <= now
            then ActivateAt scheduled
            else Don'tActivate

data ScheduledTime
  = ActivateAt UTCTime
  | Don'tActivate
  | Don'tActivateButUpdate UTCTime
  deriving (Show, Eq, Generic)

performScheduleItem :: Path Abs Dir -> ZonedTime -> ScheduleItem -> IO ScheduleItemResult
performScheduleItem wdir now ScheduleItem {..} = do
  let from = wdir </> scheduleItemTemplate
  let ctx = RenderContext {renderContextTime = now}
  case renderDestinationPathTemplate ctx scheduleItemDestination of
    Left errs -> pure $ ScheduleItemResultPathRenderError errs
    Right destination -> do
      let to = wdir </> destination
      mErrOrTemplate <- readScheduleTemplate from
      case mErrOrTemplate of
        Nothing -> pure $ ScheduleItemResultTemplateDoesNotExist from
        Just (Left err) -> pure $ ScheduleItemResultYamlParseError from err
        Just (Right template) -> do
          let vRendered = runReaderT (renderTemplate template) ctx
          case vRendered of
            Failure errs -> pure $ ScheduleItemResultFileRenderError errs
            Success rendered -> do
              destinationExists <- doesFileExist to
              if destinationExists
                then pure $ ScheduleItemResultDestinationAlreadyExists to
                else do
                  ensureDir $ parent to
                  writeSmosFile to rendered
                  pure ScheduleItemResultSuccess

data ScheduleItemResult
  = ScheduleItemResultPathRenderError (NonEmpty RenderError)
  | ScheduleItemResultTemplateDoesNotExist (Path Abs File)
  | ScheduleItemResultYamlParseError (Path Abs File) String
  | ScheduleItemResultFileRenderError (NonEmpty RenderError)
  | ScheduleItemResultDestinationAlreadyExists (Path Abs File)
  | ScheduleItemResultSuccess

scheduleItemResultMessage :: ScheduleItemResult -> Maybe String
scheduleItemResultMessage = \case
  ScheduleItemResultSuccess -> Nothing
  ScheduleItemResultPathRenderError errs ->
    Just $ unlines $
      "ERROR: Validation errors while rendering template destination file name:"
        : map prettyRenderError (NE.toList errs)
  ScheduleItemResultTemplateDoesNotExist from ->
    Just $ unwords ["ERROR: template does not exist:", fromAbsFile from]
  ScheduleItemResultYamlParseError from err ->
    Just $
      unlines
        [ unwords ["ERROR: Does not look like a smos template file:", fromAbsFile from],
          err
        ]
  ScheduleItemResultFileRenderError errs ->
    Just
      $ unlines
      $ "ERROR: Validation errors while rendering template:"
        : map prettyRenderError (NE.toList errs)
  ScheduleItemResultDestinationAlreadyExists to ->
    Just $ unwords ["WARNING: destination already exists:", fromAbsFile to, " not overwriting."]

minimumScheduleInterval :: NominalDiffTime
minimumScheduleInterval = 60 -- Only run once per minute.
