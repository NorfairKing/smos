{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.OptParse.Gen where

import Data.GenValidity

import Smos.Report.OptParse.Types

instance GenUnchecked Configuration

instance GenValid Configuration where
  genValid = genValidStructurally
