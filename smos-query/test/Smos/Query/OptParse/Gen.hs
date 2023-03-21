{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Query.OptParse.Gen where

import Data.GenValidity
import Data.GenValidity.Containers ()
import Smos.CLI.Colour
import Smos.Query.OptParse
import Smos.Report.Archive.Gen ()
import Smos.Report.OptParse.Gen ()
import Smos.Report.Report.Gen ()
import Text.Colour.Layout.Gen ()

instance GenValid Configuration where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid PreparedReportConfiguration where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ColourConfiguration where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid TableBackgroundConfiguration where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
