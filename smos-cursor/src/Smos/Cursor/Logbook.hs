module Smos.Cursor.Logbook where

import Smos.Data.Types

type LogbookCursor = Logbook

makeLogbookCursor :: Logbook -> LogbookCursor
makeLogbookCursor = id

rebuildLogbookCursor :: LogbookCursor -> Logbook
rebuildLogbookCursor = id
