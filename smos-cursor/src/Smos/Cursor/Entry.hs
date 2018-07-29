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

import Smos.Data.Types

import Smos.Cursor.Contents
import Smos.Cursor.Header

data EntryCursor = EntryCursor
    { entryCursorHeaderCursor :: HeaderCursor
    , entryCursorContentsCursor :: Maybe ContentsCursor
    , entryCursorSelected :: EntryCursorSelection
    } deriving (Show, Eq, Generic)

instance Validity EntryCursor

makeEntryCursor :: Entry -> EntryCursor
makeEntryCursor Entry {..} =
    EntryCursor
    { entryCursorHeaderCursor = makeHeaderCursor entryHeader
    , entryCursorContentsCursor = makeContentsCursor <$> entryContents
    , entryCursorSelected = WholeEntrySelected
    }

rebuildEntryCursor :: EntryCursor -> Entry
rebuildEntryCursor EntryCursor {..} =
    Entry
    { entryHeader = rebuildHeaderCursor entryCursorHeaderCursor
    , entryContents = rebuildContentsCursor <$> entryCursorContentsCursor
    }

data EntryCursorSelection
    = WholeEntrySelected
    | HeaderSelected
    | ContentsSelected
    deriving (Show, Eq, Generic)

instance Validity EntryCursorSelection
