{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Cursor.Entry
  ( EntryCursor (..),
    makeEntryCursor,
    emptyEntryCursor,
    rebuildEntryCursor,
    entryCursorHeaderCursorL,
    entryCursorContentsCursorL,
    entryCursorStateHistoryCursorL,
    entryCursorTagsCursorL,
    entryCursorPropertiesCursorL,
    entryCursorTimestampsCursorL,
    entryCursorLogbookCursorL,
    entryCursorSelectionL,
    EntryCursorSelection (..),
    entryCursorSelect,
    entryCursorSelectWhole,
    entryCursorSelectHeaderAtStart,
    entryCursorSelectHeaderAtEnd,
    entryCursorSelectContents,
    entryCursorSelectTimestamps,
    entryCursorSelectProperties,
    entryCursorSelectStateHistory,
    entryCursorSelectTags,
    entryCursorSelectLogbook,
    entryCursorUpdateTime,
  )
where

import Control.DeepSeq
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time
import Data.Validity
import GHC.Generics (Generic)
import Lens.Micro
import Smos.Cursor.Contents
import Smos.Cursor.Header
import Smos.Cursor.Logbook
import Smos.Cursor.Properties
import Smos.Cursor.StateHistory
import Smos.Cursor.Tags
import Smos.Cursor.Timestamps
import Smos.Data.Types

data EntryCursor
  = EntryCursor
      { entryCursorHeaderCursor :: HeaderCursor,
        entryCursorContentsCursor :: Maybe ContentsCursor,
        entryCursorTimestampsCursor :: Maybe TimestampsCursor,
        entryCursorPropertiesCursor :: Maybe PropertiesCursor,
        entryCursorStateHistoryCursor :: Maybe StateHistoryCursor,
        entryCursorTagsCursor :: Maybe TagsCursor,
        entryCursorLogbookCursor :: LogbookCursor,
        entryCursorSelected :: EntryCursorSelection
      }
  deriving (Show, Eq, Generic)

instance Validity EntryCursor

instance NFData EntryCursor

makeEntryCursor :: Entry -> EntryCursor
makeEntryCursor Entry {..} =
  EntryCursor
    { entryCursorHeaderCursor = makeHeaderCursor entryHeader,
      entryCursorContentsCursor = makeContentsCursor <$> entryContents,
      entryCursorTimestampsCursor = makeTimestampsCursor entryTimestamps,
      entryCursorPropertiesCursor = makePropertiesCursor <$> NE.nonEmpty (M.toList entryProperties),
      entryCursorTagsCursor = makeTagsCursor <$> NE.nonEmpty (S.toList entryTags),
      entryCursorStateHistoryCursor = makeStateHistoryCursor entryStateHistory,
      entryCursorLogbookCursor = makeLogbookCursor entryLogbook,
      entryCursorSelected = WholeEntrySelected
    }

emptyEntryCursor :: EntryCursor
emptyEntryCursor = makeEntryCursor emptyEntry

rebuildEntryCursor :: EntryCursor -> Entry
rebuildEntryCursor EntryCursor {..} =
  Entry
    { entryHeader = rebuildHeaderCursor entryCursorHeaderCursor,
      entryContents = rebuildContentsCursor <$> entryCursorContentsCursor,
      entryTimestamps = rebuildTimestampsCursor entryCursorTimestampsCursor,
      entryProperties =
        maybe M.empty (M.fromList . NE.toList) $
          rebuildPropertiesCursor <$> entryCursorPropertiesCursor,
      entryStateHistory = rebuildStateHistoryCursor entryCursorStateHistoryCursor,
      entryTags =
        maybe S.empty (S.fromList . NE.toList) $ rebuildTagsCursor <$> entryCursorTagsCursor,
      entryLogbook = rebuildLogbookCursor entryCursorLogbookCursor
    }

entryCursorSelectionL :: Lens' EntryCursor EntryCursorSelection
entryCursorSelectionL = lens entryCursorSelected $ \ec s -> ec {entryCursorSelected = s}

entryCursorHeaderCursorL :: Lens' EntryCursor HeaderCursor
entryCursorHeaderCursorL =
  lens entryCursorHeaderCursor $ \ec hc -> ec {entryCursorHeaderCursor = hc}

entryCursorContentsCursorL :: Lens' EntryCursor (Maybe ContentsCursor)
entryCursorContentsCursorL =
  lens entryCursorContentsCursor $ \ec hc -> ec {entryCursorContentsCursor = hc}

entryCursorStateHistoryCursorL :: Lens' EntryCursor (Maybe StateHistoryCursor)
entryCursorStateHistoryCursorL =
  lens entryCursorStateHistoryCursor $ \ec shc -> ec {entryCursorStateHistoryCursor = shc}

entryCursorTagsCursorL :: Lens' EntryCursor (Maybe TagsCursor)
entryCursorTagsCursorL = lens entryCursorTagsCursor $ \ec shc -> ec {entryCursorTagsCursor = shc}

entryCursorPropertiesCursorL :: Lens' EntryCursor (Maybe PropertiesCursor)
entryCursorPropertiesCursorL =
  lens entryCursorPropertiesCursor $ \ec shc -> ec {entryCursorPropertiesCursor = shc}

entryCursorTimestampsCursorL :: Lens' EntryCursor (Maybe TimestampsCursor)
entryCursorTimestampsCursorL =
  lens entryCursorTimestampsCursor $ \ec shc -> ec {entryCursorTimestampsCursor = shc}

entryCursorLogbookCursorL :: Lens' EntryCursor LogbookCursor
entryCursorLogbookCursorL =
  lens entryCursorLogbookCursor $ \ec shc -> ec {entryCursorLogbookCursor = shc}

entryCursorSelect :: EntryCursorSelection -> EntryCursor -> EntryCursor
entryCursorSelect ecl ec = ec & entryCursorSelectionL .~ ecl

entryCursorSelectWhole :: EntryCursor -> EntryCursor
entryCursorSelectWhole = entryCursorSelect WholeEntrySelected

entryCursorSelectHeaderAtStart :: EntryCursor -> EntryCursor
entryCursorSelectHeaderAtStart =
  (entryCursorHeaderCursorL %~ headerCursorSelectStart) . entryCursorSelect HeaderSelected

entryCursorSelectHeaderAtEnd :: EntryCursor -> EntryCursor
entryCursorSelectHeaderAtEnd =
  (entryCursorHeaderCursorL %~ headerCursorSelectEnd) . entryCursorSelect HeaderSelected

entryCursorSelectContents :: EntryCursor -> EntryCursor
entryCursorSelectContents =
  (entryCursorContentsCursorL %~ maybe (Just emptyContentsCursor) Just)
    . entryCursorSelect ContentsSelected

entryCursorSelectTimestamps :: EntryCursor -> EntryCursor
entryCursorSelectTimestamps = entryCursorSelect TimestampsSelected

entryCursorSelectProperties :: EntryCursor -> EntryCursor
entryCursorSelectProperties =
  (entryCursorPropertiesCursorL %~ maybe (Just emptyPropertiesCursor) Just)
    . entryCursorSelect PropertiesSelected

entryCursorSelectStateHistory :: EntryCursor -> EntryCursor
entryCursorSelectStateHistory = entryCursorSelect StateHistorySelected

entryCursorSelectTags :: EntryCursor -> EntryCursor
entryCursorSelectTags = entryCursorSelect TagsSelected

entryCursorSelectLogbook :: EntryCursor -> EntryCursor
entryCursorSelectLogbook = entryCursorSelect LogbookSelected

data EntryCursorSelection
  = WholeEntrySelected
  | HeaderSelected
  | ContentsSelected
  | TimestampsSelected
  | PropertiesSelected
  | StateHistorySelected
  | TagsSelected
  | LogbookSelected
  deriving (Show, Eq, Generic)

instance Validity EntryCursorSelection

instance NFData EntryCursorSelection

entryCursorUpdateTime :: ZonedTime -> EntryCursor -> EntryCursor
entryCursorUpdateTime zt = entryCursorTimestampsCursorL %~ fmap (timestampsCursorUpdateTime zt)
