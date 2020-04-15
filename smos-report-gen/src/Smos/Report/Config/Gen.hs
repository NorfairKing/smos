{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.Config.Gen where

import Data.GenValidity
import Data.GenValidity.Text ()
import Smos.Report.Config

instance GenValid ContextName where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
