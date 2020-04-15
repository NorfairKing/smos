{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Contents.Gen where

import Cursor.List.NonEmpty.Gen
import Cursor.Text.Gen
import Cursor.TextField
import Cursor.TextField.Gen ()
import Data.GenValidity
import Data.GenValidity.Text
import Smos.Cursor.Contents
import Smos.Data
import Test.QuickCheck

instance GenValid ContentsCursor where
  genValid =
    ContentsCursor . TextFieldCursor
      <$> genNonEmptyCursorBy
        (textCursorWithGen $ genTextCursorChar `suchThat` validContentsChar)
        (genTextBy $ genTextCursorChar `suchThat` validContentsChar)
  shrinkValid = shrinkValidStructurally
