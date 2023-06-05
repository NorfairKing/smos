{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Smos.Cursor.Timestamps
  ( TimestampsCursor (..),
    startTimestampsCursor,
    makeTimestampsCursor,
    rebuildTimestampsCursor,
    timestampsCursorToggleSelected,
    timestampsCursorInsertChar,
    timestampsCursorAppendChar,
    timestampsCursorDeleteChar,
    timestampsCursorRemoveChar,
    timestampsCursorSelectNextChar,
    timestampsCursorSelectPrevChar,
    timestampsCursorInsertAndSelect,
    timestampsCursorInsertEmptyAndSelect,
    timestampsCursorAppendAndSelect,
    timestampsCursorAppendEmptyAndSelect,
    timestampsCursorSelectOrAdd,
    timestampsCursorUpdateTime,
    makeTimestampNameCursor,
    rebuildTimestampNameCursor,
    makeTimestampCursor,
    rebuildTimestampCursor,
  )
where

import Control.DeepSeq
import Cursor.FuzzyLocalTime
import Cursor.List.NonEmpty
import Cursor.Map
import Cursor.Text
import Cursor.Types
import Data.Function
import Data.Functor.Compose
import Data.FuzzyTime
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Time
import Data.Validity
import GHC.Generics (Generic)
import Lens.Micro
import Smos.Data.Types

newtype TimestampsCursor = TimestampsCursor
  { timestampsCursorMapCursor :: MapCursor TextCursor FuzzyLocalTimeCursor TimestampName Timestamp
  }
  deriving (Show, Eq, Generic)

instance Validity TimestampsCursor where
  validate (TimestampsCursor pc) =
    mconcat
      [ delve "timestampsCursorMapCursor" pc,
        declare "The text cursor under selection builds to a valid value" $
          case nonEmptyCursorCurrent (mapCursorList pc) of
            KeyValueCursorKey tc _ -> isJust $ timestampName $ rebuildTextCursor tc
            KeyValueCursorValue _ _ -> True
      ]

instance NFData TimestampsCursor

timestampsCursorMapCursorL ::
  Lens' TimestampsCursor (MapCursor TextCursor FuzzyLocalTimeCursor TimestampName Timestamp)
timestampsCursorMapCursorL =
  lens timestampsCursorMapCursor $ \pc mc -> pc {timestampsCursorMapCursor = mc}

startTimestampsCursor :: TimestampName -> LocalTime -> TimestampsCursor
startTimestampsCursor tsn lt =
  TimestampsCursor $ singletonMapCursorValue tsn $ emptyFuzzyLocalTimeCursor (mkImpreciseLocalTime lt)

makeTimestampsCursor :: Map TimestampName Timestamp -> Maybe TimestampsCursor
makeTimestampsCursor m = do
  ne <- NE.nonEmpty $ M.toList m
  pure $ TimestampsCursor $ makeMapCursor makeTimestampNameCursor ne

rebuildTimestampsCursor :: Maybe TimestampsCursor -> Map TimestampName Timestamp
rebuildTimestampsCursor Nothing = M.empty
rebuildTimestampsCursor (Just tsc) =
  M.fromList $
    NE.toList $
      rebuildMapCursor rebuildTimestampNameCursor rebuildTimestampCursor $
        timestampsCursorMapCursor tsc

timestampsCursorCurrentTextCursorL ::
  Functor f =>
  (TextCursor -> f TextCursor) ->
  (TimestampsCursor -> f TimestampsCursor)
timestampsCursorCurrentTextCursorL =
  timestampsCursorMapCursorL
    . lens
      ( \tsc ->
          case tsc ^. mapCursorElemL of
            KeyValueCursorKey kc _ -> kc
            KeyValueCursorValue _ vc -> vc ^. fuzzyLocalTimeCursorTextCursorL
      )
      ( \tsc tc ->
          tsc
            & mapCursorElemL
              %~ ( \case
                     KeyValueCursorKey _ v -> KeyValueCursorKey tc v
                     KeyValueCursorValue k vc ->
                       KeyValueCursorValue k $ vc {fuzzyLocalTimeCursorTextCursor = tc}
                 )
      )

timestampsCursorToggleSelected :: TimestampsCursor -> TimestampsCursor
timestampsCursorToggleSelected =
  timestampsCursorMapCursorL
    %~ mapCursorToggleSelected
      rebuildTimestampNameCursor
      makeTimestampNameCursor
      rebuildTimestampCursor
      makeTimestampCursor

timestampsCursorInsertChar :: Char -> TimestampsCursor -> Maybe TimestampsCursor
timestampsCursorInsertChar c tsc =
  if validTimestampNameChar c
    then timestampsCursorCurrentTextCursorL (textCursorInsert c) tsc
    else Nothing

timestampsCursorAppendChar :: Char -> TimestampsCursor -> Maybe TimestampsCursor
timestampsCursorAppendChar c tsc =
  if validTimestampNameChar c
    then timestampsCursorCurrentTextCursorL (textCursorAppend c) tsc
    else Nothing

timestampsCursorRemoveChar :: TimestampsCursor -> Maybe (DeleteOrUpdate TimestampsCursor)
timestampsCursorRemoveChar tsc = do
  dou <-
    getCompose
      ( timestampsCursorCurrentTextCursorL
          (Compose . textCursorRemove)
          tsc
      )
  pure $ case dou of
    Deleted -> tsc & timestampsCursorMapCursorL (mapCursorRemoveElem makeTimestampNameCursor)
    Updated tsc' -> Updated tsc'

timestampsCursorDeleteChar :: TimestampsCursor -> Maybe (DeleteOrUpdate TimestampsCursor)
timestampsCursorDeleteChar tsc = do
  dou <-
    getCompose
      ( timestampsCursorCurrentTextCursorL
          (Compose . textCursorDelete)
          tsc
      )
  pure $ case dou of
    Deleted -> tsc & timestampsCursorMapCursorL (mapCursorRemoveElem makeTimestampNameCursor)
    Updated tsc' -> Updated tsc'

timestampsCursorSelectNextChar :: TimestampsCursor -> Maybe TimestampsCursor
timestampsCursorSelectNextChar = timestampsCursorCurrentTextCursorL textCursorSelectNext

timestampsCursorSelectPrevChar :: TimestampsCursor -> Maybe TimestampsCursor
timestampsCursorSelectPrevChar = timestampsCursorCurrentTextCursorL textCursorSelectPrev

timestampsCursorInsertEmptyAndSelect :: LocalTime -> TimestampsCursor -> TimestampsCursor
timestampsCursorInsertEmptyAndSelect = timestampsCursorInsertAndSelect emptyTimestampName

timestampsCursorInsertAndSelect ::
  TimestampName -> LocalTime -> TimestampsCursor -> TimestampsCursor
timestampsCursorInsertAndSelect tsn lt =
  timestampsCursorMapCursorL
    %~ mapCursorInsertAndSelectValue
      rebuildTimestampNameCursor
      rebuildTimestampCursor
      tsn
      (emptyFuzzyLocalTimeCursor (mkImpreciseLocalTime lt))

timestampsCursorAppendEmptyAndSelect :: LocalTime -> TimestampsCursor -> TimestampsCursor
timestampsCursorAppendEmptyAndSelect = timestampsCursorAppendAndSelect emptyTimestampName

timestampsCursorAppendAndSelect ::
  TimestampName -> LocalTime -> TimestampsCursor -> TimestampsCursor
timestampsCursorAppendAndSelect tsn lt =
  timestampsCursorMapCursorL
    %~ mapCursorAppendAndSelectValue
      rebuildTimestampNameCursor
      rebuildTimestampCursor
      tsn
      (emptyFuzzyLocalTimeCursor (mkImpreciseLocalTime lt))

timestampsCursorSelectOrAdd :: TimestampName -> LocalTime -> TimestampsCursor -> TimestampsCursor
timestampsCursorSelectOrAdd tsn lt =
  timestampsCursorMapCursorL
    %~ mapCursorSelectValue rebuildTimestampNameCursor makeTimestampCursor
      . mapCursorSelectOrAdd
        rebuildTimestampNameCursor
        makeTimestampNameCursor
        rebuildTimestampCursor
        (\t _ -> t == tsn)
        (makeKeyValueCursorValue tsn (emptyFuzzyLocalTimeCursor (mkImpreciseLocalTime lt)))

timestampsCursorUpdateTime :: ZonedTime -> TimestampsCursor -> TimestampsCursor
timestampsCursorUpdateTime zt = (timestampsCursorMapCursorL . mapCursorElemL) %~ go
  where
    go ::
      KeyValueCursor TextCursor FuzzyLocalTimeCursor TimestampName Timestamp ->
      KeyValueCursor TextCursor FuzzyLocalTimeCursor TimestampName Timestamp
    go kvc =
      case kvc of
        KeyValueCursorKey _ _ -> kvc
        KeyValueCursorValue k fztc ->
          KeyValueCursorValue k $
            fztc
              { fuzzyLocalTimeCursorBaseLocalTime =
                  mkImpreciseLocalTime $ utcToLocalTime (zonedTimeZone zt) (zonedTimeToUTC zt)
              }

-- safe because of validity
makeTimestampNameCursor :: TimestampName -> TextCursor
makeTimestampNameCursor = fromJust . makeTextCursor . timestampNameText

-- safe because of validity
rebuildTimestampNameCursor :: TextCursor -> TimestampName
rebuildTimestampNameCursor = fromJust . timestampName . rebuildTextCursor

makeTimestampCursor :: Timestamp -> FuzzyLocalTimeCursor
makeTimestampCursor ts =
  makeFuzzyLocalTimeCursor $
    case ts of
      TimestampDay d -> OnlyDaySpecified d
      TimestampLocalTime lt -> BothTimeAndDay lt

rebuildTimestampCursor :: FuzzyLocalTimeCursor -> Timestamp
rebuildTimestampCursor fltc =
  case rebuildFuzzyLocalTimeCursor fltc of
    OnlyDaySpecified d -> TimestampDay d
    BothTimeAndDay lt -> TimestampLocalTime (mkImpreciseLocalTime lt)
