module Smos.Cursor.Header
    ( HeaderCursor
    , makeHeaderCursor
    , rebuildHeaderCursor
    , headerCursorInsert
    , headerCursorAppend
    ) where

import Cursor.Text

import Smos.Data.Types

type HeaderCursor = TextCursor

makeHeaderCursor :: Header -> HeaderCursor
makeHeaderCursor = makeTextCursor . headerText

rebuildHeaderCursor :: HeaderCursor -> Header
rebuildHeaderCursor = Header . rebuildTextCursor

headerCursorInsert :: Char -> HeaderCursor -> HeaderCursor
headerCursorInsert = textCursorInsert
headerCursorAppend :: Char -> HeaderCursor -> HeaderCursor
headerCursorAppend = textCursorAppend
