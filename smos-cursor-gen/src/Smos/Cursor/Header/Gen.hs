{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Header.Gen where

import Data.GenValidity

import Cursor.Text.Gen

import Smos.Cursor.Header

import Smos.Data.Gen

instance GenUnchecked HeaderCursor

instance GenValid HeaderCursor where
  genValid = HeaderCursor <$> textCursorWithGen genHeaderChar
  shrinkValid = shrinkValidStructurally
