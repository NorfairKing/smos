{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.Next.Gen where

import Data.GenValidity
import Data.GenValidity.Path ()
import Smos.Data.Gen ()
import Smos.Report.Next
import Smos.Report.Path.Gen ()
import Smos.Report.Period.Gen ()
import Smos.Report.TimeBlock.Gen ()

instance GenValid NextActionReport where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid NextActionEntry where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
