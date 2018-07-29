module Smos.Cursor.Contents where

import Cursor.TextField

import Smos.Data.Types

type ContentsCursor = TextFieldCursor

makeContentsCursor :: Contents -> ContentsCursor
makeContentsCursor = makeTextFieldCursor . contentsText

rebuildContentsCursor :: ContentsCursor -> Contents
rebuildContentsCursor = Contents . rebuildTextFieldCursor
