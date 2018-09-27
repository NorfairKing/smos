module Smos.Cursor.Contents
    ( ContentsCursor(..)
    , emptyContentsCursor
    , makeContentsCursor
    , makeContentsCursorWithSelection
    , rebuildContentsCursor
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

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

import Lens.Micro

import Cursor.TextField

import Smos.Data.Types

type ContentsCursor = TextFieldCursor

emptyContentsCursor :: ContentsCursor
emptyContentsCursor = emptyTextFieldCursor

makeContentsCursor :: Contents -> ContentsCursor
makeContentsCursor = makeTextFieldCursor . contentsText

makeContentsCursorWithSelection ::
       Int -> Int -> Contents -> Maybe ContentsCursor
makeContentsCursorWithSelection x y = makeTextFieldCursorWithSelection x y . contentsText

rebuildContentsCursor :: ContentsCursor -> Contents
rebuildContentsCursor = Contents . rebuildTextFieldCursor

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

contentsCursorInsertChar :: Char -> ContentsCursor -> ContentsCursor
contentsCursorInsertChar = textFieldCursorInsertChar

contentsCursorAppendChar :: Char -> ContentsCursor -> ContentsCursor
contentsCursorAppendChar = textFieldCursorAppendChar

contentsCursorInsertNewline :: ContentsCursor -> ContentsCursor
contentsCursorInsertNewline = textFieldCursorInsertNewline

contentsCursorAppendNewline :: ContentsCursor -> ContentsCursor
contentsCursorAppendNewline = textFieldCursorAppendNewline

contentsCursorRemove :: ContentsCursor -> Maybe ContentsCursor
contentsCursorRemove = textFieldCursorRemove

contentsCursorDelete :: ContentsCursor -> Maybe ContentsCursor
contentsCursorDelete = textFieldCursorDelete

contentsCursorSelectStartOfLine :: ContentsCursor -> ContentsCursor
contentsCursorSelectStartOfLine = textFieldCursorSelectStartOfLine

contentsCursorSelectEndOfLine :: ContentsCursor -> ContentsCursor
contentsCursorSelectEndOfLine = textFieldCursorSelectEndOfLine
