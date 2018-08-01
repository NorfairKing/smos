module Smos.Cursor.Logbook.Gen where

import Data.GenValidity

import Smos.Data.Gen ()

import Cursor.NonEmpty.Gen ()

import Smos.Cursor.Logbook

instance GenUnchecked LogbookCursor

instance GenValid LogbookCursor
