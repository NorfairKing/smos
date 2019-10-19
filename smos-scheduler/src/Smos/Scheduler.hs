{-# LANGUAGE RecordWildCards #-}

module Smos.Scheduler
  ( smosScheduler
  ) where

import GHC.Generics (Generic)

import Data.Char as Char
import qualified Data.Text as T
import Data.Time
import Text.Show.Pretty

import Path
import Path.IO

import Smos.Data

import qualified Smos.Report.Config as Report

import Smos.Scheduler.OptParse
import Smos.Scheduler.OptParse.Types

smosScheduler :: IO ()
smosScheduler = getSettings >>= scheduler

scheduler :: Settings -> IO ()
scheduler sets@Settings {..} = do
  pPrint sets
  wd <- Report.resolveReportWorkflowDir setReportSettings
  mapM_ (handleScheduleItem wd) $ scheduleItems setSchedule

handleScheduleItem :: Path Abs Dir -> ScheduleItem -> IO ()
handleScheduleItem wdir ScheduleItem {..} = do
  let from = wdir </> scheduleItemTemplate
  let to = wdir </> scheduleItemDestination
  pPrint (from, to)
  templateExists <- doesFileExist from
  if templateExists
    then do
      destinationExists <- doesFileExist to
      if destinationExists
        then putStrLn $
             unwords ["WARNING: destination already exists:", fromAbsFile to, "not overwriting."]
        else do
          ensureDir $ parent to
          copyFile from to
    else putStrLn $ unwords ["WARNING: template does not exist:", fromAbsFile from]
