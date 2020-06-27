{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Scheduler.Render.Gen where

import Data.GenValidity.Containers ()
import Data.GenValidity.Time ()
import Smos.Data.Gen ()
import Smos.Scheduler.OptParse.Types
import Smos.Scheduler.Render
import Test.Validity

instance GenValid ScheduleTemplate where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid RenderContext where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid EntryTemplate where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid TimestampTemplate where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid StateHistoryTemplate where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid StateHistoryEntryTemplate where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid UTCTimeTemplate where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
