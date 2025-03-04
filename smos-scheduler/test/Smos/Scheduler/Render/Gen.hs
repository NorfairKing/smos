{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Scheduler.Render.Gen where

import Data.GenValidity.Time ()
import Smos.Report.Time.Gen ()
import Smos.Scheduler.Render
import Test.Syd.Validity

instance GenValid RenderContext where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
