{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Cursor.Entry
    ( EntryCursor(..)
    , makeEntryCursor
    , rebuildEntryCursor
    , EntryCursorSelection(..)
    ) where

import GHC.Generics (Generic)

import Data.Validity

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M

import Smos.Data.Types

import Smos.Cursor.Contents
import Smos.Cursor.Header
import Smos.Cursor.Properties
import Smos.Cursor.Tags
import Smos.Cursor.Timestamps

data EntryCursor = EntryCursor
    { entryCursorHeaderCursor :: HeaderCursor
    , entryCursorContentsCursor :: Maybe ContentsCursor
    , entryCursorSelected :: EntryCursorSelection
    , entryCursorTimestampsCursor :: Maybe TimestampsCursor
    , entryCursorPropertiesCursor :: Maybe PropertiesCursor
    , entryCursorTagsCursor :: Maybe TagsCursor
    } deriving (Show, Eq, Generic)

instance Validity EntryCursor

makeEntryCursor :: Entry -> EntryCursor
makeEntryCursor Entry {..} =
    EntryCursor
    { entryCursorHeaderCursor = makeHeaderCursor entryHeader
    , entryCursorContentsCursor = makeContentsCursor <$> entryContents
    , entryCursorTimestampsCursor =
          makeTimestampsCursor <$> NE.nonEmpty (M.toList entryTimestamps)
    , entryCursorPropertiesCursor =
          makePropertiesCursor <$> NE.nonEmpty (M.toList entryProperties)
    , entryCursorTagsCursor = makeTagsCursor <$> NE.nonEmpty entryTags
    , entryCursorSelected = WholeEntrySelected
    }

rebuildEntryCursor :: EntryCursor -> Entry
rebuildEntryCursor EntryCursor {..} =
    Entry
    { entryHeader = rebuildHeaderCursor entryCursorHeaderCursor
    , entryContents = rebuildContentsCursor <$> entryCursorContentsCursor
    , entryTimestamps =
          maybe M.empty (M.fromList . NE.toList) $
          rebuildTimestampsCursor <$> entryCursorTimestampsCursor
    , entryProperties =
          maybe M.empty (M.fromList . NE.toList) $
          rebuildPropertiesCursor <$> entryCursorPropertiesCursor
    , entryTags =
          maybe [] NE.toList $ rebuildTagsCursor <$> entryCursorTagsCursor
    }

data EntryCursorSelection
    = WholeEntrySelected
    | HeaderSelected
    | ContentsSelected
    | TimestampsSelected
    | PropertiesSelected
    | TagsSelected
    deriving (Show, Eq, Generic)

instance Validity EntryCursorSelection
