{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Scheduler.Template.Gen where

import Data.GenValidity
import Data.GenValidity.Text ()
import Smos.Scheduler.Template

instance GenValid Template where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid TemplatePiece where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
