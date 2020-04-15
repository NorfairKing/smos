{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Tags.Gen where

import Cursor.List.NonEmpty.Gen ()
import Cursor.Text.Gen ()
import Data.GenValidity
import Smos.Cursor.Tag.Gen ()
import Smos.Cursor.Tags
import Smos.Data.Gen ()

instance GenValid TagsCursor where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
