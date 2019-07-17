{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Keys.Gen where

import TestImport

import Graphics.Vty.Input.Events

import Smos.Data.Gen ()
import Smos.Report.OptParse.Gen ()

import Smos.Keys

instance GenValid Key where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Modifier where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid KeyPress where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
