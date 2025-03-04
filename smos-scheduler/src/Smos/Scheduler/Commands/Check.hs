{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Scheduler.Commands.Check (check) where

import Control.Monad.Logger
import Control.Monad.Writer
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Path
import Path.IO
import Smos.CLI.Logging
import Smos.Directory.Resolution
import Smos.Scheduler.OptParse
import Smos.Scheduler.Render
import Smos.Scheduler.Schedule
import System.Exit

check :: Settings -> IO ()
check Settings {..} = runFilteredLogger setLogLevel $ do
  wd <- liftIO $ resolveDirWorkflowDir setDirectorySettings
  errs <- execWriterT $ scheduleCheck wd setSchedule
  case NE.nonEmpty errs of
    Nothing -> pure ()
    Just ne -> do
      logErrorN $ T.pack $ unlines $ "Schedule is invalid:" : map renderScheduleError (NE.toList ne)
      liftIO exitFailure

type Check a = WriterT [ScheduleError] (LoggingT IO) a

data ScheduleError
  = ScheduleErrorDestinationTemplate !DestinationPathTemplate !(NonEmpty RenderError)
  | ScheduleErrorMissingTemplateFile !(Path Abs File)
  | ScheduleErrorUnreadableTemplateFile !(Path Abs File) !String
  | ScheduleErrorTemplateRender !(Path Abs File) !(NonEmpty RenderError)

renderScheduleError :: ScheduleError -> String
renderScheduleError = \case
  ScheduleErrorDestinationTemplate tf errs ->
    unlines $
      unwords
        [ "Failed to render a destination file template:",
          fromRelFile (destinationPathTemplatePath tf)
        ]
        : map prettyRenderError (NE.toList errs)
  ScheduleErrorMissingTemplateFile f ->
    unwords
      [ "Template file does not exist:",
        fromAbsFile f
      ]
  ScheduleErrorUnreadableTemplateFile f err ->
    unlines
      [ unwords
          [ "Error reading template file:",
            fromAbsFile f
          ],
        err
      ]
  ScheduleErrorTemplateRender f errs ->
    unlines $
      unwords
        [ "Error while rendering template:",
          fromAbsFile f
        ]
        : map show (NE.toList errs)

emitScheduleError :: ScheduleError -> Check ()
emitScheduleError = tell . pure

scheduleCheck :: Path Abs Dir -> Schedule -> Check ()
scheduleCheck wd (Schedule sis) = mapM_ (scheduleItemCheck wd) sis

scheduleItemCheck :: Path Abs Dir -> ScheduleItem -> Check ()
scheduleItemCheck wd ScheduleItem {..} = do
  scheduleItemDestinationCheck scheduleItemDestination
  scheduleItemTemplateCheck wd scheduleItemTemplateFile

scheduleItemDestinationCheck :: DestinationPathTemplate -> Check ()
scheduleItemDestinationCheck tf = do
  errOrRendered <- liftIO $ runRenderNow $ renderDestinationPathTemplate tf
  case errOrRendered of
    Left errs -> emitScheduleError $ ScheduleErrorDestinationTemplate tf errs
    Right _ -> pure ()

scheduleItemTemplateCheck :: Path Abs Dir -> Text -> Check ()
scheduleItemTemplateCheck wd tf = do
  f <- resolveFile wd $ T.unpack tf
  mErrOrTemplate <- liftIO $ readScheduleTemplate f
  case mErrOrTemplate of
    Nothing -> emitScheduleError $ ScheduleErrorMissingTemplateFile f
    Just (Left err) -> emitScheduleError $ ScheduleErrorUnreadableTemplateFile f err
    Just (Right template) -> do
      errOrRendered <- liftIO $ runRenderNow $ renderTemplate template
      case errOrRendered of
        Left errs -> emitScheduleError $ ScheduleErrorTemplateRender f errs
        Right _ -> pure ()
