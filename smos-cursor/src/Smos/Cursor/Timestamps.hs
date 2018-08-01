module Smos.Cursor.Timestamps
    ( TimestampsCursor
    , makeTimestampsCursor
    , rebuildTimestampsCursor
    ) where

import Data.List.NonEmpty (NonEmpty)

import Cursor.Map

import Smos.Data.Types

type TimestampsCursor = MapCursor TimestampName Timestamp

makeTimestampsCursor :: NonEmpty (TimestampName, Timestamp) -> TimestampsCursor
makeTimestampsCursor = makeMapCursor

rebuildTimestampsCursor ::
       TimestampsCursor -> NonEmpty (TimestampName, Timestamp)
rebuildTimestampsCursor = rebuildMapCursor
