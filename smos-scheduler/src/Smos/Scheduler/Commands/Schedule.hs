{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Scheduler.Commands.Schedule
  ( schedule,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Time
import Path
import Path.IO
import Smos.Data
import Smos.Report.Config as Report
import Smos.Scheduler.OptParse
import Smos.Scheduler.Recurrence
import Smos.Scheduler.Render
import Smos.Scheduler.Utils

schedule :: Settings -> IO ()
schedule Settings {..} = do
  now <- getZonedTime
  rh <- readReccurrenceHistory setDirectorySettings
  handleSchedule setDirectorySettings rh now setSchedule

handleSchedule :: DirectoryConfig -> RecurrenceHistory -> ZonedTime -> Schedule -> IO ()
handleSchedule dc rh now sched =
  mapM_ (handleScheduleItem dc rh now) (scheduleItems sched)

handleScheduleItem :: DirectoryConfig -> RecurrenceHistory -> ZonedTime -> ScheduleItem -> IO (Maybe UTCTime)
handleScheduleItem dc rh now si = do
  case computeNextRun rh (zonedTimeToUTC now) si of
    Nothing -> do
      putStrLn $
        unwords
          [ "Not activating",
            scheduleItemDisplayName si,
            "because",
            case scheduleItemRecurrence si of
              RentRecurrence _ -> "it will never be activated again."
              HaircutRecurrence _ -> "it is still in progress."
          ]
      pure Nothing
    Just timeToActivate ->
      if timeToActivate > zonedTimeToUTC now
        then do
          putStrLn $
            unwords
              [ "Not activating",
                scheduleItemDisplayName si,
                "because it should not be activated before",
                show timeToActivate
              ]
          pure Nothing
        else do
          r <- performScheduleItem dc (utcToZonedTime (zonedTimeZone now) timeToActivate) si
          case scheduleItemResultMessage r of
            Nothing -> do
              putStrLn $
                unwords
                  [ "Succesfully activated",
                    scheduleItemDisplayName si
                  ]
              pure $ Just timeToActivate
            Just msg -> do
              putStrLn msg
              pure Nothing

scheduleItemDisplayName :: ScheduleItem -> String
scheduleItemDisplayName si@ScheduleItem {..} =
  maybe
    (unwords ["the item with schedule", show scheduleItemRecurrence, "and hash", show (hashScheduleItem si)])
    show
    scheduleItemDescription

performScheduleItem :: DirectoryConfig -> ZonedTime -> ScheduleItem -> IO ScheduleItemResult
performScheduleItem dc now si@ScheduleItem {..} = do
  wdir <- Report.resolveDirWorkflowDir dc
  let from = wdir </> scheduleItemTemplate
  let ctx = RenderContext {renderContextTime = now}
  case runRender ctx $ renderDestinationPathTemplate scheduleItemDestination of
    Left errs -> pure $ ScheduleItemResultPathRenderError errs
    Right destination -> do
      let to = wdir </> destination
      mErrOrTemplate <- readScheduleTemplate from
      case mErrOrTemplate of
        Nothing -> pure $ ScheduleItemResultTemplateDoesNotExist from
        Just (Left err) -> pure $ ScheduleItemResultYamlParseError from err
        Just (Right template) -> do
          let errOrRendered = runRender ctx $ renderTemplate template
          case errOrRendered of
            Left errs -> pure $ ScheduleItemResultFileRenderError errs
            Right rendered -> do
              destinationExists <- doesFileExist to
              if destinationExists
                then pure $ ScheduleItemResultDestinationAlreadyExists to
                else do
                  let renderedWithMetadata = addScheduleHashMetadata (hashScheduleItem si) rendered
                  ensureDir $ parent to
                  writeSmosFile to renderedWithMetadata
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
    Just $
      unlines $
        "ERROR: Validation errors while rendering template destination file name:" :
        map prettyRenderError (NE.toList errs)
  ScheduleItemResultTemplateDoesNotExist from ->
    Just $ unwords ["ERROR: template does not exist:", fromAbsFile from]
  ScheduleItemResultYamlParseError from err ->
    Just $
      unlines
        [ unwords ["ERROR: Does not look like a smos template file:", fromAbsFile from],
          err
        ]
  ScheduleItemResultFileRenderError errs ->
    Just $
      unlines $
        "ERROR: Validation errors while rendering template:" :
        map prettyRenderError (NE.toList errs)
  ScheduleItemResultDestinationAlreadyExists to ->
    Just $ unwords ["WARNING: destination already exists:", fromAbsFile to, " not overwriting."]
