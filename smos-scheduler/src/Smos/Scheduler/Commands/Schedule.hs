{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Scheduler.Commands.Schedule
  ( schedule,
    scheduleAsIfAt,
    handleScheduleItem,
    ScheduleItemResult (..),
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Zones
import Path
import Path.IO
import Smos.Data
import Smos.Directory.OptParse
import Smos.Directory.Resolution
import Smos.Scheduler.History
import Smos.Scheduler.OptParse
import Smos.Scheduler.Recurrence
import Smos.Scheduler.Render
import Smos.Scheduler.Schedule

schedule :: Settings -> IO ()
schedule settings = do
  now <- getCurrentTime
  scheduleAsIfAt now settings

scheduleAsIfAt :: UTCTime -> Settings -> IO ()
scheduleAsIfAt now Settings {..} = do
  zone <- loadLocalTZ
  rh <- readReccurrenceHistory setDirectorySettings zone
  handleSchedule setDirectorySettings zone rh now setSchedule

handleSchedule :: DirectorySettings -> TZ -> RecurrenceHistory -> UTCTime -> Schedule -> IO ()
handleSchedule dc zone rh now sched =
  mapM_ (uncurry (handleScheduleItem dc zone rh now)) (M.toList (scheduleItems sched))

handleScheduleItem :: DirectorySettings -> TZ -> RecurrenceHistory -> UTCTime -> ScheduleItemName -> ScheduleItem -> IO (Maybe LocalTime)
handleScheduleItem dc zone rh now sn si = do
  let activateImmediately :: IO (Maybe LocalTime)
      activateImmediately = activateAsIfAt (utcToLocalTimeTZ zone now)
      displayName = show @Text $ scheduleItemDisplayName sn si
      activateAsIfAt :: LocalTime -> IO (Maybe LocalTime)
      activateAsIfAt time = do
        r <- performScheduleItem dc time sn si
        case scheduleItemResultMessage r of
          Nothing -> do
            putStrLn $
              unwords
                [ "Succesfully activated",
                  displayName
                ]
            pure $ Just time
          Just msg -> do
            putStrLn msg
            pure Nothing
  case computeNextRun zone now rh sn si of
    Left hnr -> case hnr of
      DoNotActivateHaircut -> do
        putStrLn $
          unwords
            [ "Not activating",
              displayName,
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
                  displayName,
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
              displayName,
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
                  displayName,
                  "because it should not be activated before",
                  show timeToActivate
                ]
            pure Nothing
          else activateAsIfAt timeToActivate

scheduleItemDisplayName :: ScheduleItemName -> ScheduleItem -> Text
scheduleItemDisplayName sn ScheduleItem {..} =
  fromMaybe
    (propertyValueText sn)
    scheduleItemDescription

performScheduleItem :: DirectorySettings -> LocalTime -> ScheduleItemName -> ScheduleItem -> IO ScheduleItemResult
performScheduleItem dc pretendTime sn ScheduleItem {..} = do
  wdir <- resolveDirWorkflowDir dc
  from <- resolveFile wdir $ T.unpack scheduleItemTemplateFile
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
                  let renderedWithMetadata = addScheduleHashMetadata pretendTime sn rendered
                  ensureDir $ parent to
                  writeSmosFile to renderedWithMetadata
                  pure ScheduleItemResultSuccess

data ScheduleItemResult
  = ScheduleItemResultPathRenderError !(NonEmpty RenderError)
  | ScheduleItemResultTemplateDoesNotExist !(Path Abs File)
  | ScheduleItemResultYamlParseError !(Path Abs File) !String
  | ScheduleItemResultFileRenderError !(NonEmpty RenderError)
  | ScheduleItemResultDestinationAlreadyExists !(Path Abs File)
  | ScheduleItemResultSuccess
  deriving (Show, Eq)

scheduleItemResultMessage :: ScheduleItemResult -> Maybe String
scheduleItemResultMessage = \case
  ScheduleItemResultSuccess -> Nothing
  ScheduleItemResultPathRenderError errs ->
    Just $
      unlines $
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
    Just $
      unlines $
        "ERROR: Validation errors while rendering template:"
          : map prettyRenderError (NE.toList errs)
  ScheduleItemResultDestinationAlreadyExists to ->
    Just $ unwords ["WARNING: destination already exists:", fromAbsFile to, " not overwriting."]
