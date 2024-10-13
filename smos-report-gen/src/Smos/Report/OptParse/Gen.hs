{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.OptParse.Gen where

import Data.GenValidity
import Data.GenValidity.Map ()
import Smos.Report.Filter.Gen ()
import Smos.Report.OptParse
import Smos.Report.Projection.Gen ()
import Smos.Report.Sorter.Gen ()

instance GenValid ContextName where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
