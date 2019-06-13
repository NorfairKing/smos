{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Properties.Gen where

import Data.GenValidity

import Cursor.Map.Gen ()
import Cursor.Text.Gen ()

import Smos.Data.Gen ()

import Smos.Cursor.Properties

instance GenValid PropertiesCursor where
    genValid = genValidStructurally
    shrinkValid = shrinkValidStructurally
