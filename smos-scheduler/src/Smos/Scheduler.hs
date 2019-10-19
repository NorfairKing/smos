module Smos.Scheduler
  ( smosScheduler
  ) where

import Data.Char as Char
import qualified Data.Text as T
import Data.Time
import Path
import Text.Show.Pretty

import Smos.Data

import Smos.Report.Config

import Smos.Scheduler.OptParse
import Smos.Scheduler.OptParse.Types

smosScheduler :: IO ()
smosScheduler = getSettings >>= scheduler

scheduler :: Settings -> IO ()
scheduler = pPrint
