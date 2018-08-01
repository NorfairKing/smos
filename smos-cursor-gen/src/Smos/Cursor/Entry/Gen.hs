module Smos.Cursor.Entry.Gen where

import Data.GenValidity

import Test.QuickCheck

import Smos.Cursor.Entry

import Smos.Cursor.Contents.Gen ()
import Smos.Cursor.Header.Gen ()
import Smos.Cursor.Logbook.Gen ()
import Smos.Cursor.Properties.Gen ()
import Smos.Cursor.Tags.Gen ()
import Smos.Cursor.Timestamps.Gen ()

instance GenUnchecked EntryCursor

instance GenValid EntryCursor where
    genValid =
        (EntryCursor <$> genValid <*> genValid <*> genValid <*> genValid <*> genValid <*>
         genValid <*>
         genValid <*>
         genValid) `suchThat`
        isValid

instance GenUnchecked EntryCursorSelection

instance GenValid EntryCursorSelection
