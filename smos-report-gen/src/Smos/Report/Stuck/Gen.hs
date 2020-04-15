{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.Stuck.Gen where

import Data.GenValidity
import Data.GenValidity.Path ()
import Smos.Data.Gen ()
import Smos.Report.Path.Gen ()
import Smos.Report.Period.Gen ()
import Smos.Report.Stuck
import Smos.Report.TimeBlock.Gen ()

instance GenValid StuckReport where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid StuckReportEntry where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
