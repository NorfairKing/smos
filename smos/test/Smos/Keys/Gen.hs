{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Keys.Gen where

import Graphics.Vty.Input.Events
import Smos.Data.Gen ()
import Smos.Keys
import Smos.Report.OptParse.Gen ()
import TestImport

instance GenValid Key where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Modifier where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid KeyPress where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid MatcherConfig where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
