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
  , timestampsCursorUpdateTime
  , makeTimestampNameCursor
  , rebuildTimestampNameCursor
  , makeTimestampCursor
  , rebuildTimestampCursor
  ) where

import Data.Function
import Data.FuzzyTime
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Time
import Lens.Micro

import Cursor.FuzzyLocalTime
import Cursor.Map
import Cursor.Text
import Cursor.Types

import Smos.Data.Types

type TimestampsCursor = MapCursor TextCursor FuzzyLocalTimeCursor TimestampName Timestamp

startTimestampsCursor :: TimestampName -> LocalTime -> TimestampsCursor
startTimestampsCursor tsn lt = singletonMapCursorValue tsn $ emptyFuzzyLocalTimeCursor lt

makeTimestampsCursor :: Map TimestampName Timestamp -> Maybe TimestampsCursor
makeTimestampsCursor m = do
  ne <- NE.nonEmpty $ M.toList m
  pure $ makeMapCursor makeTimestampNameCursor ne

rebuildTimestampsCursor :: Maybe TimestampsCursor -> Map TimestampName Timestamp
rebuildTimestampsCursor Nothing = M.empty
rebuildTimestampsCursor (Just tsc) =
  M.fromList $ NE.toList $ rebuildMapCursor rebuildTimestampNameCursor rebuildTimestampCursor tsc

timestampsCursorCurrentTextCursorL :: Lens' TimestampsCursor TextCursor
timestampsCursorCurrentTextCursorL =
  lens
    (\tsc ->
       case tsc ^. mapCursorElemL of
         KeyValueCursorKey kc _ -> kc
         KeyValueCursorValue _ vc -> vc ^. fuzzyLocalTimeCursorTextCursorL)
    (\tsc tc ->
       tsc &
       mapCursorElemL %~
       (\kvc ->
          case kvc of
            KeyValueCursorKey _ v -> KeyValueCursorKey tc v
            KeyValueCursorValue k vc ->
              KeyValueCursorValue k $ vc {fuzzyLocalTimeCursorTextCursor = tc}))

timestampsCursorToggleSelected :: TimestampsCursor -> TimestampsCursor
timestampsCursorToggleSelected =
  mapCursorToggleSelected
    rebuildTimestampNameCursor
    makeTimestampNameCursor
    rebuildTimestampCursor
    makeTimestampCursor

timestampsCursorInsertChar :: Char -> TimestampsCursor -> Maybe TimestampsCursor
timestampsCursorInsertChar c = timestampsCursorCurrentTextCursorL $ textCursorInsert c

timestampsCursorAppendChar :: Char -> TimestampsCursor -> Maybe TimestampsCursor
timestampsCursorAppendChar c = timestampsCursorCurrentTextCursorL $ textCursorAppend c

timestampsCursorRemoveChar :: TimestampsCursor -> Maybe TimestampsCursor
timestampsCursorRemoveChar = timestampsCursorCurrentTextCursorL (dullMDelete . textCursorRemove)

timestampsCursorDeleteChar :: TimestampsCursor -> Maybe TimestampsCursor
timestampsCursorDeleteChar = timestampsCursorCurrentTextCursorL (dullMDelete . textCursorDelete)

timestampsCursorSelectNextChar :: TimestampsCursor -> Maybe TimestampsCursor
timestampsCursorSelectNextChar = timestampsCursorCurrentTextCursorL textCursorSelectNext

timestampsCursorSelectPrevChar :: TimestampsCursor -> Maybe TimestampsCursor
timestampsCursorSelectPrevChar = timestampsCursorCurrentTextCursorL textCursorSelectPrev

timestampsCursorInsertEmptyAndSelect :: LocalTime -> TimestampsCursor -> TimestampsCursor
timestampsCursorInsertEmptyAndSelect = timestampsCursorInsertAndSelect emptyTimestampName

timestampsCursorInsertAndSelect ::
     TimestampName -> LocalTime -> TimestampsCursor -> TimestampsCursor
timestampsCursorInsertAndSelect tsn lt =
  mapCursorInsertAndSelectValue rebuildTimestampNameCursor rebuildTimestampCursor tsn $
  emptyFuzzyLocalTimeCursor lt

timestampsCursorAppendEmptyAndSelect :: LocalTime -> TimestampsCursor -> TimestampsCursor
timestampsCursorAppendEmptyAndSelect = timestampsCursorAppendAndSelect emptyTimestampName

timestampsCursorAppendAndSelect ::
     TimestampName -> LocalTime -> TimestampsCursor -> TimestampsCursor
timestampsCursorAppendAndSelect tsn d =
  mapCursorAppendAndSelectValue rebuildTimestampNameCursor rebuildTimestampCursor tsn $
  emptyFuzzyLocalTimeCursor d

timestampsCursorSelectOrAdd :: TimestampName -> LocalTime -> TimestampsCursor -> TimestampsCursor
timestampsCursorSelectOrAdd tsn d =
  mapCursorSelectOrAdd
    rebuildTimestampNameCursor
    makeTimestampNameCursor
    rebuildTimestampCursor
    (\t _ -> t == tsn)
    (makeKeyValueCursorValue tsn (emptyFuzzyLocalTimeCursor d))

timestampsCursorUpdateTime :: ZonedTime -> TimestampsCursor -> TimestampsCursor
timestampsCursorUpdateTime zt = mapCursorElemL %~ go
  where
    go ::
         KeyValueCursor TextCursor FuzzyLocalTimeCursor TimestampName Timestamp
      -> KeyValueCursor TextCursor FuzzyLocalTimeCursor TimestampName Timestamp
    go kvc =
      case kvc of
        KeyValueCursorKey _ _ -> kvc
        KeyValueCursorValue k fztc ->
          KeyValueCursorValue k $ fztc {fuzzyLocalTimeCursorBaseLocalTime = utcToLocalTime (zonedTimeZone zt) (zonedTimeToUTC zt)}

-- safe because of validity
makeTimestampNameCursor :: TimestampName -> TextCursor
makeTimestampNameCursor = fromJust . makeTextCursor . timestampNameText

-- safe because of validity
rebuildTimestampNameCursor :: TextCursor -> TimestampName
rebuildTimestampNameCursor = fromJust . timestampName . rebuildTextCursor

makeTimestampCursor :: Timestamp -> FuzzyLocalTimeCursor
makeTimestampCursor = makeFuzzyLocalTimeCursor . timestampLocalTime

rebuildTimestampCursor :: FuzzyLocalTimeCursor -> Timestamp
rebuildTimestampCursor fltc =
  case rebuildFuzzyLocalTimeCursor fltc of
    OnlyDaySpecified d -> TimestampDay d
    BothTimeAndDay lt -> TimestampLocalTime lt
