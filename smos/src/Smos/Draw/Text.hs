{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smos.Draw.Text
  ( drawTextFieldCursor
  , drawTextCursor
  ) where

import Data.Tuple

import Brick.Types as B
import Brick.Widgets.Core as B

import Cursor.Text
import Cursor.TextField

import Smos.Draw.Base
import Smos.Style
import Smos.Types

drawTextFieldCursor :: Select -> TextFieldCursor -> Widget ResourceName
drawTextFieldCursor s tfc =
  (case s of
     MaybeSelected ->
       visible .
       showCursor
         textCursorName
         (B.Location (swap (textFieldCursorSelection tfc)))
     _ -> id) $
  drawText $ rebuildTextFieldCursor tfc

drawTextCursor :: Select -> TextCursor -> Widget ResourceName
drawTextCursor s tc =
  (case s of
     MaybeSelected ->
       visible . showCursor textCursorName (B.Location (textCursorIndex tc, 0))
     _ -> id) $
  drawText $ rebuildTextCursor tc
