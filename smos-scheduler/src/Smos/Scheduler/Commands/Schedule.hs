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
import Data.Time.Zones
import Path
import Path.IO
import Smos.Data
import Smos.Directory.Config
import Smos.Scheduler.OptParse
import Smos.Scheduler.Recurrence
import Smos.Scheduler.Render

schedule :: Settings -> IO ()
schedule Settings {..} = do
  now <- getCurrentTime
  rh <- readReccurrenceHistory setDirectorySettings
  handleSchedule setDirectorySettings rh now setSchedule

handleSchedule :: DirectoryConfig -> RecurrenceHistory -> UTCTime -> Schedule -> IO ()
handleSchedule dc rh now sched =
  mapM_ (handleScheduleItem dc rh now) (scheduleItems sched)

handleScheduleItem :: DirectoryConfig -> RecurrenceHistory -> UTCTime -> ScheduleItem -> IO (Maybe LocalTime)
handleScheduleItem dc rh now si = do
  zone <- loadLocalTZ
  let activateImmediately :: IO (Maybe LocalTime)
      activateImmediately = activateAsIfAt (utcToLocalTimeTZ zone now)
      activateAsIfAt :: LocalTime -> IO (Maybe LocalTime)
      activateAsIfAt time = do
        r <- performScheduleItem dc time si
        case scheduleItemResultMessage r of
          Nothing -> do
            putStrLn $
              unwords
                [ "Succesfully activated",
                  scheduleItemDisplayName si
                ]
            pure $ Just time
          Just msg -> do
            putStrLn msg
            pure Nothing
  case computeNextRun zone now rh si of
    Left hnr -> case hnr of
      DoNotActivateHaircut -> do
        putStrLn $
          unwords
            [ "Not activating",
              scheduleItemDisplayName si,
              "because it is still in progress."
            ]
        pure Nothing
      ActivateHaircutImmediately -> activateImmediately
      ActivateHaircutNoSoonerThan timeToActivate ->
        if timeToActivate > now
          then do
            putStrLn $
              unwords
                [ "Not activating",
                  scheduleItemDisplayName si,
                  "because it should not be activated before",
                  show (utcToLocalTimeTZ zone timeToActivate)
                ]
            pure Nothing
          else activateImmediately
    Right rnr -> case rnr of
      DoNotActivateRent -> do
        putStrLn $
          unwords
            [ "Not activating",
              scheduleItemDisplayName si,
              "because it will never be activated (again)."
            ]
        pure Nothing
      ActivateRentImmediatelyAsIfAt next -> activateAsIfAt next
      ActivateRentNoSoonerThan timeToActivate ->
        if timeToActivate > utcToLocalTimeTZ zone now
          then do
            putStrLn $
              unwords
                [ "Not activating",
                  scheduleItemDisplayName si,
                  "because it should not be activated before",
                  show timeToActivate
                ]
            pure Nothing
          else activateAsIfAt timeToActivate

scheduleItemDisplayName :: ScheduleItem -> String
scheduleItemDisplayName si@ScheduleItem {..} =
  maybe
    (unwords ["the item with schedule", show scheduleItemRecurrence, "and hash", show (hashScheduleItem si)])
    show
    scheduleItemDescription

performScheduleItem :: DirectoryConfig -> LocalTime -> ScheduleItem -> IO ScheduleItemResult
performScheduleItem dc pretendTime si@ScheduleItem {..} = do
  wdir <- resolveDirWorkflowDir dc
  from <- resolveFile wdir scheduleItemTemplate
  errOrRendered <- runRenderAsIfAt pretendTime $ renderDestinationPathTemplate scheduleItemDestination
  case errOrRendered of
    Left errs -> pure $ ScheduleItemResultPathRenderError errs
    Right destination -> do
      let to = wdir </> destination
      mErrOrTemplate <- readScheduleTemplate from
      case mErrOrTemplate of
        Nothing -> pure $ ScheduleItemResultTemplateDoesNotExist from
        Just (Left err) -> pure $ ScheduleItemResultYamlParseError from err
        Just (Right template) -> do
          errOrRendered' <- runRenderAsIfAt pretendTime $ renderTemplate template
          case errOrRendered' of
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
