{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Scheduler.Render.Gen where

import Data.GenValidity.Containers ()
import Data.GenValidity.Time ()
import Smos.Data.Gen ()
import Smos.Scheduler.OptParse.Types
import Smos.Scheduler.Render
import Test.Syd.Validity

instance GenValid RenderContext where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid ScheduleTemplate where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid EntryTemplate where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid TimestampTemplate where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid UTCTimeTemplate where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
