module Smos.Cursor.Entry.Gen where

import Data.GenValidity

import Smos.Cursor.Entry

import Smos.Cursor.Contents.Gen ()
import Smos.Cursor.Header.Gen ()

instance GenUnchecked EntryCursor

instance GenValid EntryCursor

instance GenUnchecked EntryCursorSelection

instance GenValid EntryCursorSelection
