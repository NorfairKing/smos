{-# LANGUAGE OverloadedStrings #-}

module Smos.Cursor.Contents
    ( ContentsCursor
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

import qualified Data.Text as T

import Cursor.TextField

import Smos.Data.Types

type ContentsCursor = TextFieldCursor

emptyContentsCursor :: ContentsCursor
emptyContentsCursor = emptyTextFieldCursor

makeContentsCursor :: Contents -> ContentsCursor
makeContentsCursor = makeTextFieldCursor . contentsText

makeContentsCursorWithSelection ::
       Int -> Int -> Contents -> Maybe ContentsCursor
makeContentsCursorWithSelection x y =
    makeTextFieldCursorWithSelection x y . contentsText

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

contentsCursorInsertChar :: Char -> Maybe ContentsCursor -> ContentsCursor
contentsCursorInsertChar c mcc =
    case mcc of
        Nothing -> makeTextFieldCursor (T.pack [c])
        Just cc -> textFieldCursorInsertChar c cc

contentsCursorAppendChar :: Char -> Maybe ContentsCursor -> ContentsCursor
contentsCursorAppendChar c mcc =
    case mcc of
        Nothing -> makeTextFieldCursor (T.pack [c])
        Just cc -> textFieldCursorAppendChar c cc

contentsCursorInsertNewline :: Maybe ContentsCursor -> ContentsCursor
contentsCursorInsertNewline mcc =
    case mcc of
        Nothing -> makeTextFieldCursor "\n"
        Just cc -> textFieldCursorInsertNewline cc

contentsCursorAppendNewline :: Maybe ContentsCursor -> ContentsCursor
contentsCursorAppendNewline mcc =
    case mcc of
        Nothing -> makeTextFieldCursor "\n"
        Just cc -> textFieldCursorAppendNewline cc

contentsCursorRemove :: ContentsCursor -> Maybe ContentsCursor
contentsCursorRemove = textFieldCursorRemove

contentsCursorDelete :: ContentsCursor -> Maybe ContentsCursor
contentsCursorDelete = textFieldCursorDelete

contentsCursorSelectStartOfLine :: ContentsCursor -> ContentsCursor
contentsCursorSelectStartOfLine = textFieldCursorSelectStartOfLine

contentsCursorSelectEndOfLine :: ContentsCursor -> ContentsCursor
contentsCursorSelectEndOfLine = textFieldCursorSelectEndOfLine
