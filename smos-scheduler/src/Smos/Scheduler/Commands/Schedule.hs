{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Scheduler.Commands.Schedule
  ( schedule,
  )
where

import Data.Hashable
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
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
  handleSchedule setDirectorySettings now setSchedule

handleSchedule :: DirectoryConfig -> ZonedTime -> Schedule -> IO ()
handleSchedule dc now sched =
  mapM_ (handleScheduleItem dc now) (scheduleItems sched)

handleScheduleItem :: DirectoryConfig -> ZonedTime -> ScheduleItem -> IO (Maybe UTCTime)
handleScheduleItem dc now si = do
  scheduledTime <- computeScheduledTime dc (zonedTimeToUTC now) si
  case scheduledTime of
    Don'tActivate -> do
      putStrLn $ unwords ["Not activating", T.unpack (scheduleItemDisplayName si)]
      pure Nothing
    ActivateAt timeToActivate -> do
      r <- performScheduleItem dc (utcToZonedTime (zonedTimeZone now) timeToActivate) si
      case scheduleItemResultMessage r of
        Nothing -> do
          putStrLn $ "Succesfully activated " <> T.unpack (scheduleItemDisplayName si)
          pure $ Just timeToActivate
        Just msg -> do
          putStrLn msg
          pure Nothing

scheduleItemDisplayName :: ScheduleItem -> Text
scheduleItemDisplayName si@ScheduleItem {..} =
  maybe
    (T.unwords ["the item with schedule", T.pack (show scheduleItemRecurrence), "and hash", T.pack (show (hashScheduleItem si))])
    (T.pack . show)
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
                  let renderedWithMetadata = addMetadata (hash si) rendered
                  ensureDir $ parent to
                  writeSmosFile to renderedWithMetadata
                  pure ScheduleItemResultSuccess

addMetadata :: Int -> SmosFile -> SmosFile
addMetadata h sf = makeSmosFile $ goF (smosFileForest sf)
  where
    goF :: Forest Entry -> Forest Entry
    goF = \case
      [] -> []
      (t : rest) -> goT t : rest
    goT :: Tree Entry -> Tree Entry
    goT (Node e sub) = Node (goE e) sub
    goE :: Entry -> Entry
    goE e =
      let mpv = propertyValue $ T.pack (show h)
       in case mpv of
            Nothing -> e
            Just pv ->
              e
                { entryProperties =
                    M.insert
                      scheduleHashPropertyName
                      pv
                      (entryProperties e)
                }

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
