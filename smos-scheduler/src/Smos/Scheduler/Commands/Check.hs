{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Scheduler.Commands.Check
  ( check,
  )
where

import qualified Data.List.NonEmpty as NE
import Path
import Path.IO
import qualified Smos.Report.Config as Report
import Smos.Scheduler.OptParse
import Smos.Scheduler.Render
import System.Exit

check :: Settings -> IO ()
check Settings {..} = do
  wd <- Report.resolveDirWorkflowDir setDirectorySettings
  scheduleCheck wd setSchedule

scheduleCheck :: Path Abs Dir -> Schedule -> IO ()
scheduleCheck wd (Schedule sis) = mapM_ (scheduleItemCheck wd) sis

scheduleItemCheck :: Path Abs Dir -> ScheduleItem -> IO ()
scheduleItemCheck wd ScheduleItem {..} = do
  ctx <- loadRenderContext
  scheduleItemTemplateCheck wd ctx scheduleItemTemplate
  scheduleItemDestinationCheck ctx scheduleItemDestination

scheduleItemTemplateCheck :: Path Abs Dir -> RenderContext -> FilePath -> IO ()
scheduleItemTemplateCheck wd ctx tf = do
  f <- resolveFile wd tf
  mErrOrTemplate <- readScheduleTemplate f
  case mErrOrTemplate of
    Nothing -> die $ "Template file does not exist: " <> fromAbsFile f
    Just (Left err) -> die $ unlines [unwords ["Error reading template file:", fromAbsFile f], err]
    Just (Right template) -> do
      let errOrRendered = runRender ctx $ renderTemplate template
      case errOrRendered of
        Left errs -> die $ unlines $ "Error while rendering template: " : map show (NE.toList errs)
        Right _ -> pure ()

scheduleItemDestinationCheck :: RenderContext -> DestinationPathTemplate -> IO ()
scheduleItemDestinationCheck ctx tf = do
  case runRender ctx $ renderDestinationPathTemplate tf of
    Left errs -> die $ unlines $ unwords ["Failed to render a destination file template: ", fromRelFile (destinationPathTemplatePath tf)] : map prettyRenderError (NE.toList errs)
    Right _ -> pure ()
