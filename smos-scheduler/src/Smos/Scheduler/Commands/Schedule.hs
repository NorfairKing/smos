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

import Control.Monad.IO.Class
import Control.Monad.Logger
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
import Smos.CLI.Logging
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
scheduleAsIfAt now Settings {..} = runFilteredLogger setLogLevel $ do
  rh <- liftIO $ readReccurrenceHistory setDirectorySettings
  handleSchedule setDirectorySettings rh now setSchedule

handleSchedule :: DirectorySettings -> RecurrenceHistory -> UTCTime -> Schedule -> LoggingT IO ()
handleSchedule dc rh now sched = do
  zone <- liftIO loadLocalTZ
  let nowLocal = utcToLocalTimeTZ zone now
  mapM_ (uncurry (handleScheduleItem dc rh nowLocal)) (M.toList (scheduleItems sched))

handleScheduleItem ::
  DirectorySettings ->
  RecurrenceHistory ->
  LocalTime ->
  ScheduleItemName ->
  ScheduleItem ->
  LoggingT IO (Maybe LocalTime)
handleScheduleItem dc rh nowLocal sn si = do
  let activateImmediately = activateAsIfAt nowLocal
      displayName =
        show @Text $
          fromMaybe
            (propertyValueText sn)
            (scheduleItemDescription si)
      activateAsIfAt :: LocalTime -> LoggingT IO (Maybe LocalTime)
      activateAsIfAt time = do
        r <- liftIO $ performScheduleItem dc time sn si
        logResult displayName r
        case r of
          ScheduleItemResultSuccess -> pure $ Just time
          _ -> pure Nothing
  case computeNextRun nowLocal rh sn si of
    Left hnr -> case hnr of
      DoNotActivateHaircut -> do
        logInfoN $
          T.pack $
            unwords
              [ "Not activating",
                displayName,
                "because it is still in progress."
              ]
        pure Nothing
      ActivateHaircutImmediately -> activateImmediately
      ActivateHaircutNoSoonerThan timeToActivate ->
        if timeToActivate > nowLocal
          then do
            logInfoN $
              T.pack $
                unwords
                  [ "Not activating",
                    displayName,
                    "because it should not be activated before",
                    show timeToActivate
                  ]
            pure Nothing
          else activateImmediately
    Right rnr -> case rnr of
      DoNotActivateRent -> do
        logInfoN $
          T.pack $
            unwords
              [ "Not activating",
                displayName,
                "because it will never be activated (again)."
              ]
        pure Nothing
      ActivateRentImmediatelyAsIfAt next -> activateAsIfAt next
      ActivateRentNoSoonerThan timeToActivate ->
        if timeToActivate > nowLocal
          then do
            logInfoN $
              T.pack $
                unwords
                  [ "Not activating",
                    displayName,
                    "because it should not be activated before",
                    show timeToActivate
                  ]
            pure Nothing
          else activateAsIfAt timeToActivate

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
                  let renderedWithMetadata = addScheduleMetadata pretendTime sn rendered
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

logResult :: String -> ScheduleItemResult -> LoggingT IO ()
logResult displayName = \case
  ScheduleItemResultSuccess ->
    logInfoN $
      T.pack $
        unwords
          [ "Succesfully activated",
            displayName
          ]
  ScheduleItemResultPathRenderError errs ->
    logErrorN $
      T.pack $
        unlines $
          "Validation errors while rendering template destination file name:"
            : map prettyRenderError (NE.toList errs)
  ScheduleItemResultTemplateDoesNotExist from ->
    logErrorN $
      T.pack $
        unwords ["template does not exist:", fromAbsFile from]
  ScheduleItemResultYamlParseError from err ->
    logErrorN $
      T.pack $
        unlines
          [ unwords ["Does not look like a smos template file:", fromAbsFile from],
            err
          ]
  ScheduleItemResultFileRenderError errs ->
    logErrorN $
      T.pack $
        unlines $
          "Validation errors while rendering template:"
            : map prettyRenderError (NE.toList errs)
  ScheduleItemResultDestinationAlreadyExists to ->
    logWarnN $
      T.pack $
        unwords
          [ "WARNING: destination already exists:",
            fromAbsFile to,
            " not overwriting."
          ]
