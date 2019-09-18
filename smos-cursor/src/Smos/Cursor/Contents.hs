module Smos.Cursor.Contents
  ( ContentsCursor
  , makeContentsCursor
  , makeContentsCursorWithSelection
  , rebuildContentsCursor
  , emptyContentsCursor
  , nullContentsCursor
  , contentsCursorSelection
  , contentsCursorSelectPrevLine
  , contentsCursorSelectNextLine
  , contentsCursorSelectFirstLine
  , contentsCursorSelectLastLine
  , contentsCursorSelectPrevChar
  , contentsCursorSelectNextChar
  , contentsCursorIndexOnLine
  , contentsCursorSelectIndexOnLine
  , contentsCursorInsertChar
  , contentsCursorAppendChar
  , contentsCursorInsertNewline
  , contentsCursorAppendNewline
  , contentsCursorRemove
  , contentsCursorDelete
  , contentsCursorSelectStartOfLine
  , contentsCursorSelectEndOfLine
  ) where

import Cursor.TextField
import Cursor.Types

import Smos.Data.Types

type ContentsCursor = TextFieldCursor

makeContentsCursor :: Contents -> ContentsCursor
makeContentsCursor = makeTextFieldCursor . contentsText

makeContentsCursorWithSelection ::
     Int -> Int -> Contents -> Maybe ContentsCursor
makeContentsCursorWithSelection x y =
  makeTextFieldCursorWithSelection x y . contentsText

rebuildContentsCursor :: ContentsCursor -> Contents
rebuildContentsCursor = Contents . rebuildTextFieldCursor

emptyContentsCursor :: ContentsCursor
emptyContentsCursor = emptyTextFieldCursor

nullContentsCursor :: ContentsCursor -> Bool
nullContentsCursor = nullTextFieldCursor

contentsCursorSelection :: ContentsCursor -> (Int, Int)
contentsCursorSelection = textFieldCursorSelection

contentsCursorSelectPrevLine :: ContentsCursor -> Maybe ContentsCursor
contentsCursorSelectPrevLine = textFieldCursorSelectPrevLine

contentsCursorSelectNextLine :: ContentsCursor -> Maybe ContentsCursor
contentsCursorSelectNextLine = textFieldCursorSelectNextLine

contentsCursorSelectFirstLine :: ContentsCursor -> ContentsCursor
contentsCursorSelectFirstLine = textFieldCursorSelectFirstLine

contentsCursorSelectLastLine :: ContentsCursor -> ContentsCursor
contentsCursorSelectLastLine = textFieldCursorSelectLastLine

contentsCursorSelectPrevChar :: ContentsCursor -> Maybe ContentsCursor
contentsCursorSelectPrevChar = textFieldCursorSelectPrevChar

contentsCursorSelectNextChar :: ContentsCursor -> Maybe ContentsCursor
contentsCursorSelectNextChar = textFieldCursorSelectNextChar

contentsCursorIndexOnLine :: ContentsCursor -> Int
contentsCursorIndexOnLine = textFieldCursorIndexOnLine

contentsCursorSelectIndexOnLine :: Int -> ContentsCursor -> ContentsCursor
contentsCursorSelectIndexOnLine = textFieldCursorSelectIndexOnLine

contentsCursorInsertChar :: Char -> Maybe ContentsCursor -> ContentsCursor
contentsCursorInsertChar = textFieldCursorInsertChar

contentsCursorAppendChar :: Char -> Maybe ContentsCursor -> ContentsCursor
contentsCursorAppendChar = textFieldCursorAppendChar

contentsCursorInsertNewline :: Maybe ContentsCursor -> ContentsCursor
contentsCursorInsertNewline = textFieldCursorInsertNewline

contentsCursorAppendNewline :: Maybe ContentsCursor -> ContentsCursor
contentsCursorAppendNewline = textFieldCursorAppendNewline

contentsCursorRemove :: ContentsCursor -> Maybe (DeleteOrUpdate ContentsCursor)
contentsCursorRemove = textFieldCursorRemove

contentsCursorDelete :: ContentsCursor -> Maybe (DeleteOrUpdate ContentsCursor)
contentsCursorDelete = textFieldCursorDelete

contentsCursorSelectStartOfLine :: ContentsCursor -> ContentsCursor
contentsCursorSelectStartOfLine = textFieldCursorSelectStartOfLine

contentsCursorSelectEndOfLine :: ContentsCursor -> ContentsCursor
contentsCursorSelectEndOfLine = textFieldCursorSelectEndOfLine
