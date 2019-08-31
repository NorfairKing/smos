{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.OptParse.Gen where

import Data.GenValidity
import Data.GenValidity.Map ()

import Smos.Report.OptParse.Types

import Smos.Report.Config.Gen ()
import Smos.Report.Filter.Gen ()

instance GenValid Configuration where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
