{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main as Criterion
import Data.GenValidity.Criterion
import Smos.Cursor.Collapse
import Smos.Cursor.Collapse.Gen ()
import Smos.Cursor.Contents
import Smos.Cursor.Contents.Gen ()
import Smos.Cursor.Entry
import Smos.Cursor.Entry.Gen ()
import Smos.Cursor.FileBrowser
import Smos.Cursor.FileBrowser.Gen ()
import Smos.Cursor.Header
import Smos.Cursor.Header.Gen ()
import Smos.Cursor.Logbook
import Smos.Cursor.Logbook.Gen ()
import Smos.Cursor.Properties
import Smos.Cursor.Properties.Gen ()
import Smos.Cursor.SmosFile
import Smos.Cursor.SmosFile.Gen ()
import Smos.Cursor.StateHistory
import Smos.Cursor.StateHistory.Gen ()
import Smos.Cursor.Tag
import Smos.Cursor.Tag.Gen ()
import Smos.Cursor.Tags
import Smos.Cursor.Tags.Gen ()
import Smos.Cursor.Timestamps
import Smos.Cursor.Timestamps.Gen ()
import Smos.Data
import Smos.Data.Gen ()
import Smos.History
import Smos.History.Gen ()
import Smos.Undo
import Smos.Undo.Gen ()

main :: IO ()
main =
  Criterion.defaultMain
    [ genValidBench @HeaderCursor,
      genValidBench @ContentsCursor,
      genValidBench @TagCursor,
      genValidBench @TagsCursor,
      genValidBench @TimestampsCursor,
      genValidBench @LogbookCursor,
      genValidBench @PropertiesCursor,
      genValidBench @StateHistoryCursor,
      genValidBench @(CollapseEntry Entry),
      genValidBench @EntryCursor,
      genValidBench @(CollapseEntry EntryCursor),
      genValidBench @SmosFileCursor,
      genValidBench @(UndoStack Int),
      genValidBench @FileBrowserCursor,
      genValidBench @(History Int)
    ]
