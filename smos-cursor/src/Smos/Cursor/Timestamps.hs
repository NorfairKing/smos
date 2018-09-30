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
    ) where

import Data.Function
import Data.List.NonEmpty (NonEmpty)
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

type TimestampsCursor = MapCursor TextCursor FuzzyDayCursor

startTimestampsCursor :: TimestampName -> Day -> TimestampsCursor
startTimestampsCursor tsn d =
    mapCursorSelectValue $
    singletonMapCursor
        (textCursorSelectEnd $ fromJust $ makeTextCursor $ timestampNameText tsn) $
    emptyFuzzyDayCursor d

makeTimestampsCursor :: Map TimestampName Timestamp -> Maybe TimestampsCursor
makeTimestampsCursor m = do
    ne <- NE.nonEmpty $ M.toList m
    let ne' =
            NE.map
                (\(tsn, ts)
                        -- Safe because of validity
                  ->
                     ( fromJust $ makeTextCursor $ timestampNameText tsn
                     , makeFuzzyDayCursor $ timestampDay ts))
                ne
    pure $ makeMapCursor ne'

rebuildTimestampsCursor :: Maybe TimestampsCursor -> Map TimestampName Timestamp
rebuildTimestampsCursor Nothing = M.empty
rebuildTimestampsCursor (Just tsc) =
    M.fromList $ NE.toList $ NE.map go $ rebuildMapCursor tsc
  where
    go :: (TextCursor, FuzzyDayCursor) -> (TimestampName, Timestamp)
    go (tc, fdc)
        -- Safe because fo validity
     =
        ( fromJust $ timestampName $ rebuildTextCursor tc
        , Timestamp $ rebuildFuzzyDayCursor fdc)

timestampsCursorCurrentTextCursorL :: Lens' TimestampsCursor TextCursor
timestampsCursorCurrentTextCursorL tcFunc tsc =
    (case keyValueCursorToggle $ tsc ^. mapCursorElemL of
         KeySelected -> mapCursorElemL . keyValueCursorKeyL
         ValueSelected ->
             mapCursorElemL . keyValueCursorValueL . fuzzyDayCursorTextCursorL)
        tcFunc
        tsc

timestampsCursorToggleSelected :: TimestampsCursor -> TimestampsCursor
timestampsCursorToggleSelected = mapCursorToggleSelected

timestampsCursorInsertChar :: Char -> TimestampsCursor -> Maybe TimestampsCursor
timestampsCursorInsertChar c =
    timestampsCursorCurrentTextCursorL $ textCursorInsert c

timestampsCursorAppendChar :: Char -> TimestampsCursor -> Maybe TimestampsCursor
timestampsCursorAppendChar c =
    timestampsCursorCurrentTextCursorL $ textCursorAppend c

timestampsCursorRemoveChar :: TimestampsCursor -> Maybe TimestampsCursor
timestampsCursorRemoveChar = timestampsCursorCurrentTextCursorL textCursorRemove

timestampsCursorDeleteChar :: TimestampsCursor -> Maybe TimestampsCursor
timestampsCursorDeleteChar = timestampsCursorCurrentTextCursorL textCursorDelete

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
    mapCursorInsertAndSelect (fromJust $ makeTextCursor $ timestampNameText tsn) $
    emptyFuzzyDayCursor d

timestampsCursorAppendEmptyAndSelect ::
       Day -> TimestampsCursor -> TimestampsCursor
timestampsCursorAppendEmptyAndSelect =
    timestampsCursorAppendAndSelect emptyTimestampName

timestampsCursorAppendAndSelect ::
       TimestampName -> Day -> TimestampsCursor -> TimestampsCursor
timestampsCursorAppendAndSelect tsn d =
    mapCursorAppendAndSelect (fromJust $ makeTextCursor $ timestampNameText tsn) $
    emptyFuzzyDayCursor d

timestampsCursorSelectOrAdd ::
       TimestampName -> Day -> TimestampsCursor -> TimestampsCursor
timestampsCursorSelectOrAdd tsn d =
    mapCursorSelectOrAdd
        (\tc _ -> rebuildTextCursor tc == timestampNameText tsn)
        KeyValueCursor
             { keyValueCursorKey =
                   fromJust $ makeTextCursor $ timestampNameText tsn
             , keyValueCursorValue = emptyFuzzyDayCursor d
             , keyValueCursorToggle = ValueSelected
             }
