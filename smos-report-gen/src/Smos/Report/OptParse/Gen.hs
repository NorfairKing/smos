{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.OptParse.Gen where

import Data.GenValidity
import Data.GenValidity.Map ()
import Smos.Report.Config.Gen ()
import Smos.Report.Filter.Gen ()
import Smos.Report.OptParse.Types

instance GenValid Configuration where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
