{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Tag.Gen where

import Data.GenValidity

import Cursor.List.NonEmpty.Gen ()
import Cursor.Text.Gen

import Smos.Data.Gen

import Smos.Cursor.Tag

instance GenUnchecked TagCursor

instance GenValid TagCursor where
  genValid = TagCursor <$> textCursorWithGen genTagChar
  shrinkValid = shrinkValidStructurally
