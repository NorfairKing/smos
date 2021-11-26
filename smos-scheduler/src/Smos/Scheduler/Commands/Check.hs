{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Scheduler.Commands.Check
  ( check,
  )
where

import qualified Data.List.NonEmpty as NE
import Data.Time
import Path
import qualified Smos.Report.Config as Report
import Smos.Scheduler.OptParse
import Smos.Scheduler.Render
import Smos.Scheduler.Utils
import System.Exit

check :: Settings -> IO ()
check Settings {..} = do
  wd <- Report.resolveDirWorkflowDir setDirectorySettings
  stateFileCheck setStateFile
  scheduleCheck wd setSchedule

stateFileCheck :: Path Abs File -> IO ()
stateFileCheck sf = do
  _ <- readStateFile sf
  pure ()

scheduleCheck :: Path Abs Dir -> Schedule -> IO ()
scheduleCheck wd (Schedule sis) = mapM_ (scheduleItemCheck wd) sis

scheduleItemCheck :: Path Abs Dir -> ScheduleItem -> IO ()
scheduleItemCheck wd ScheduleItem {..} = do
  scheduleItemTemplateCheck wd scheduleItemTemplate
  scheduleItemDestinationCheck wd scheduleItemDestination

scheduleItemTemplateCheck :: Path Abs Dir -> Path Rel File -> IO ()
scheduleItemTemplateCheck wd tf = do
  let f = wd </> tf
  mErrOrTemplate <- readScheduleTemplate f
  case mErrOrTemplate of
    Nothing -> die $ "Template file does not exist: " <> fromAbsFile f
    Just (Left err) -> die $ unlines [unwords ["Error reading template file:", fromAbsFile f], err]
    Just (Right template) -> do
      now <- getZonedTime
      let ctx = RenderContext {renderContextTime = now}
      let errOrRendered = runRender ctx $ renderTemplate template
      case errOrRendered of
        Left errs -> die $ unlines $ "Error while rendering template: " : map show (NE.toList errs)
        Right _ -> pure ()

scheduleItemDestinationCheck :: Path Abs Dir -> DestinationPathTemplate -> IO ()
scheduleItemDestinationCheck _ tf = do
  now <- getZonedTime
  let ctx = RenderContext {renderContextTime = now}
  case renderDestinationPathTemplate ctx tf of
    Left errs -> die $ unlines $ unwords ["Failed to render a destination file template: ", fromRelFile (destinationPathTemplatePath tf)] : map prettyRenderError (NE.toList errs)
    Right _ -> pure ()
