{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Entry.Gen where

import Data.GenValidity

import Test.QuickCheck

import Smos.Cursor.Entry

import Smos.Cursor.Contents.Gen ()
import Smos.Cursor.Header.Gen ()
import Smos.Cursor.Logbook.Gen ()
import Smos.Cursor.Properties.Gen ()
import Smos.Cursor.StateHistory.Gen ()
import Smos.Cursor.Tags.Gen ()
import Smos.Cursor.Timestamps.Gen ()

instance GenUnchecked EntryCursor where
    genUnchecked =
        sized $ \n -> do
            (x, y) <- genSplit n
            (a, b, c, d) <- genSplit4 x
            (e, f, g, h) <- genSplit4 y
            entryCursorHeaderCursor <- resize a genUnchecked
            entryCursorContentsCursor <- resize b genUnchecked
            entryCursorTimestampsCursor <- resize c genUnchecked
            entryCursorPropertiesCursor <- resize d genUnchecked
            entryCursorStateHistoryCursor <- resize e genUnchecked
            entryCursorTagsCursor <- resize f genUnchecked
            entryCursorLogbookCursor <- resize g genUnchecked
            entryCursorSelected <- resize h genUnchecked
            pure EntryCursor {..}

instance GenValid EntryCursor where
    genValid =
        sized $ \n -> do
            (x, y) <- genSplit n
            (a, b, c, d) <- genSplit4 x
            (e, f, g, h) <- genSplit4 y
            entryCursorHeaderCursor <- resize a genValid
            entryCursorContentsCursor <- resize b genValid
            entryCursorTimestampsCursor <- resize c genValid
            entryCursorPropertiesCursor <- resize d genValid
            entryCursorStateHistoryCursor <- resize e genValid
            entryCursorTagsCursor <- resize f genValid
            entryCursorLogbookCursor <- resize g genValid
            entryCursorSelected <- resize h genValid
            pure EntryCursor {..}
    shrinkValid = shrinkValidStructurally

instance GenUnchecked EntryCursorSelection

instance GenValid EntryCursorSelection where
    genValid = genValidStructurally
