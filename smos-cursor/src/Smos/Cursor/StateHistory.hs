module Smos.Cursor.StateHistory
    ( StateHistoryCursor
    , makeStateHistoryCursor
    , rebuildStateHistoryCursor
    ) where

import Data.List.NonEmpty (NonEmpty)

import Cursor.NonEmpty

import Smos.Data.Types

type StateHistoryCursor = NonEmptyCursor StateHistoryEntry

makeStateHistoryCursor :: NonEmpty StateHistoryEntry -> StateHistoryCursor
makeStateHistoryCursor = makeNonEmptyCursor

rebuildStateHistoryCursor :: StateHistoryCursor -> NonEmpty StateHistoryEntry
rebuildStateHistoryCursor = rebuildNonEmptyCursor
