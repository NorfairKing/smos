module Smos.Cursor.Header
    ( HeaderCursor
    , makeHeaderCursor
    , rebuildHeaderCursor
    ) where

import Cursor.Text

import Smos.Data.Types

type HeaderCursor = TextCursor

makeHeaderCursor :: Header -> HeaderCursor
makeHeaderCursor = makeTextCursor . headerText

rebuildHeaderCursor :: HeaderCursor -> Header
rebuildHeaderCursor = Header . rebuildTextCursor
