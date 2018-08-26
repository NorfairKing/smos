module Smos.Cursor.Header
    ( HeaderCursor
    , makeHeaderCursor
    , rebuildHeaderCursor
    , headerCursorInsert
    , headerCursorAppend
    , headerCursorRemove
    , headerCursorDelete
    , headerCursorSelectPrev
    , headerCursorSelectNext
    , headerCursorSelectStart
    , headerCursorSelectEnd
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

headerCursorRemove :: HeaderCursor -> Maybe HeaderCursor
headerCursorRemove = textCursorRemove

headerCursorDelete :: HeaderCursor -> Maybe HeaderCursor
headerCursorDelete = textCursorDelete

headerCursorSelectPrev :: HeaderCursor -> Maybe HeaderCursor
headerCursorSelectPrev = textCursorSelectPrev

headerCursorSelectNext :: HeaderCursor -> Maybe HeaderCursor
headerCursorSelectNext = textCursorSelectNext

headerCursorSelectStart :: HeaderCursor -> HeaderCursor
headerCursorSelectStart = textCursorSelectStart

headerCursorSelectEnd :: HeaderCursor -> HeaderCursor
headerCursorSelectEnd = textCursorSelectEnd
