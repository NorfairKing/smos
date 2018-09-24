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

import Data.Maybe

import Smos.Data.Types

type HeaderCursor = TextCursor

-- fromJust is safe because makeTextCursor only works with text without newlines,
-- and that is one of the validity requirements of 'Header'.
makeHeaderCursor :: Header -> HeaderCursor
makeHeaderCursor = fromJust . makeTextCursor . headerText

-- fromJust is safe because 'header' only returns Nothing if the text cursor contains
-- newlines and it's one of the validity constraints that it doesn't.
rebuildHeaderCursor :: HeaderCursor -> Header
rebuildHeaderCursor = fromJust . header . rebuildTextCursor

headerCursorInsert :: Char -> HeaderCursor -> Maybe HeaderCursor
headerCursorInsert = textCursorInsert

headerCursorAppend :: Char -> HeaderCursor -> Maybe HeaderCursor
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
