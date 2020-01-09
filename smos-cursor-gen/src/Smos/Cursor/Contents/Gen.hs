{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Contents.Gen where

import Data.GenValidity
import Data.GenValidity.Text

import Cursor.List.NonEmpty.Gen
import Cursor.Text.Gen
import Cursor.TextField
import Cursor.TextField.Gen ()

import Test.QuickCheck

import Smos.Cursor.Contents

import Smos.Data

instance GenValid ContentsCursor where
  genValid =
    ContentsCursor . TextFieldCursor <$>
    genNonEmptyCursorBy
      (textCursorWithGen $ genTextCursorChar `suchThat` validContentsChar)
      (genTextBy $ genTextCursorChar `suchThat` validContentsChar)
  shrinkValid = shrinkValidStructurally
