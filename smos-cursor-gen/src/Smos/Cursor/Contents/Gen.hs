{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Contents.Gen where

import Data.GenValidity
import Data.GenValidity.Text

import Cursor.List.NonEmpty
import Cursor.Text.Gen
import Cursor.TextField
import Cursor.TextField.Gen

import Test.QuickCheck

import Smos.Cursor.Contents

import Smos.Data
import Smos.Data.Gen

instance GenValid ContentsCursor where
  genValid =
    fmap ContentsCursor $
    sized $ \n -> do
      (a, b, c) <- genSplit3 n
      prevs <- resize a $ genListOf $ genTextBy (genTextCursorChar `suchThat` validContentsChar)
      nexts <- resize b $ genListOf $ genTextBy (genTextCursorChar `suchThat` validContentsChar)
      cur <- resize c $ textCursorWithGen (genTextCursorChar `suchThat` validContentsChar)
      let nec = NonEmptyCursor prevs cur nexts
      pure $ TextFieldCursor nec
  shrinkValid = shrinkValidStructurally
