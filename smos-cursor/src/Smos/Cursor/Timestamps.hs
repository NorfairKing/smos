module Smos.Cursor.Timestamps
    ( TimestampsCursor
    , startTimestampsCursor
    , makeTimestampsCursor
    , rebuildTimestampsCursor
    , timestampsCursorCurrentTextCursorL
    , timestampsCursorToggleSelected
    , timestampsCursorInsertChar
    , timestampsCursorAppendChar
    , timestampsCursorDeleteChar
    , timestampsCursorRemoveChar
    , timestampsCursorSelectNextChar
    , timestampsCursorSelectPrevChar
    , timestampsCursorInsertAndSelect
    , timestampsCursorInsertEmptyAndSelect
    , timestampsCursorAppendAndSelect
    , timestampsCursorAppendEmptyAndSelect
    , timestampsCursorSelectOrAdd
    , makeTimestampNameCursor
    , rebuildTimestampNameCursor
    , makeTimestampCursor
    , rebuildTimestampCursor
    ) where

import Data.Function
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Time
import Lens.Micro

import Cursor.FuzzyDay
import Cursor.Map
import Cursor.Text
import Cursor.Types

import Smos.Data.Types

type TimestampsCursor
     = MapCursor TextCursor FuzzyDayCursor TimestampName Timestamp

startTimestampsCursor :: TimestampName -> Day -> TimestampsCursor
startTimestampsCursor tsn d =
    singletonMapCursorValue tsn $ emptyFuzzyDayCursor d

makeTimestampsCursor :: Map TimestampName Timestamp -> Maybe TimestampsCursor
makeTimestampsCursor m = do
    ne <- NE.nonEmpty $ M.toList m
    pure $ makeMapCursor makeTimestampNameCursor ne

rebuildTimestampsCursor :: Maybe TimestampsCursor -> Map TimestampName Timestamp
rebuildTimestampsCursor Nothing = M.empty
rebuildTimestampsCursor (Just tsc) =
    M.fromList $
    NE.toList $
    rebuildMapCursor rebuildTimestampNameCursor rebuildTimestampCursor tsc

timestampsCursorCurrentTextCursorL :: Lens' TimestampsCursor TextCursor
timestampsCursorCurrentTextCursorL =
    lens
        (\tsc ->
             case tsc ^. mapCursorElemL of
                 KeyValueCursorKey kc _ -> kc
                 KeyValueCursorValue _ vc -> vc ^. fuzzyDayCursorTextCursorL)
        (\tsc tc ->
             tsc &
             mapCursorElemL %~
             (\kvc ->
                  case kvc of
                      KeyValueCursorKey _ v -> KeyValueCursorKey tc v
                      KeyValueCursorValue k vc ->
                          KeyValueCursorValue k $
                          vc {fuzzyDayCursorTextCursor = tc}))

timestampsCursorToggleSelected :: TimestampsCursor -> TimestampsCursor
timestampsCursorToggleSelected =
    mapCursorToggleSelected
        rebuildTimestampNameCursor
        makeTimestampNameCursor
        rebuildTimestampCursor
        makeTimestampCursor

timestampsCursorInsertChar :: Char -> TimestampsCursor -> Maybe TimestampsCursor
timestampsCursorInsertChar c =
    timestampsCursorCurrentTextCursorL $ textCursorInsert c

timestampsCursorAppendChar :: Char -> TimestampsCursor -> Maybe TimestampsCursor
timestampsCursorAppendChar c =
    timestampsCursorCurrentTextCursorL $ textCursorAppend c

timestampsCursorRemoveChar :: TimestampsCursor -> Maybe TimestampsCursor
timestampsCursorRemoveChar =
    timestampsCursorCurrentTextCursorL (dullMDelete . textCursorRemove)

timestampsCursorDeleteChar :: TimestampsCursor -> Maybe TimestampsCursor
timestampsCursorDeleteChar =
    timestampsCursorCurrentTextCursorL (dullMDelete . textCursorDelete)

timestampsCursorSelectNextChar :: TimestampsCursor -> Maybe TimestampsCursor
timestampsCursorSelectNextChar =
    timestampsCursorCurrentTextCursorL textCursorSelectNext

timestampsCursorSelectPrevChar :: TimestampsCursor -> Maybe TimestampsCursor
timestampsCursorSelectPrevChar =
    timestampsCursorCurrentTextCursorL textCursorSelectPrev

timestampsCursorInsertEmptyAndSelect ::
       Day -> TimestampsCursor -> TimestampsCursor
timestampsCursorInsertEmptyAndSelect =
    timestampsCursorInsertAndSelect emptyTimestampName

timestampsCursorInsertAndSelect ::
       TimestampName -> Day -> TimestampsCursor -> TimestampsCursor
timestampsCursorInsertAndSelect tsn d =
    mapCursorInsertAndSelectValue
        rebuildTimestampNameCursor
        rebuildTimestampCursor
        tsn $
    emptyFuzzyDayCursor d

timestampsCursorAppendEmptyAndSelect ::
       Day -> TimestampsCursor -> TimestampsCursor
timestampsCursorAppendEmptyAndSelect =
    timestampsCursorAppendAndSelect emptyTimestampName

timestampsCursorAppendAndSelect ::
       TimestampName -> Day -> TimestampsCursor -> TimestampsCursor
timestampsCursorAppendAndSelect tsn d =
    mapCursorAppendAndSelectValue
        rebuildTimestampNameCursor
        rebuildTimestampCursor
        tsn $
    emptyFuzzyDayCursor d

timestampsCursorSelectOrAdd ::
       TimestampName -> Day -> TimestampsCursor -> TimestampsCursor
timestampsCursorSelectOrAdd tsn d =
    mapCursorSelectOrAdd
        rebuildTimestampNameCursor
        makeTimestampNameCursor
        rebuildTimestampCursor
        (\t _ -> t == tsn)
        (makeKeyValueCursorValue tsn (emptyFuzzyDayCursor d))

-- safe because of validity
makeTimestampNameCursor :: TimestampName -> TextCursor
makeTimestampNameCursor = fromJust . makeTextCursor . timestampNameText

-- safe because of validity
rebuildTimestampNameCursor :: TextCursor -> TimestampName
rebuildTimestampNameCursor = fromJust . timestampName . rebuildTextCursor

makeTimestampCursor :: Timestamp -> FuzzyDayCursor
makeTimestampCursor = makeFuzzyDayCursor . timestampDay

rebuildTimestampCursor :: FuzzyDayCursor -> Timestamp
rebuildTimestampCursor = Timestamp . rebuildFuzzyDayCursor
