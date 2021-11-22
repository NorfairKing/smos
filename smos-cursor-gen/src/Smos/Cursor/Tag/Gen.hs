{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Tag.Gen where

import Cursor.List.NonEmpty.Gen ()
import Cursor.Text.Gen
import Data.GenValidity
import Smos.Cursor.Tag
import Smos.Data.Gen

instance GenValid TagCursor where
  genValid = TagCursor <$> textCursorWithGen genTagChar
  shrinkValid = shrinkValidStructurally
