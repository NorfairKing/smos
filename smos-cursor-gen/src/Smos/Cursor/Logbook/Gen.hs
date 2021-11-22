{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Logbook.Gen where

import Cursor.Simple.List.NonEmpty.Gen ()
import Data.GenValidity
import Smos.Cursor.Logbook
import Smos.Data.Gen ()

instance GenValid LogbookCursor where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
