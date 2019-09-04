module Smos.Cursor.Header.Gen where

import Data.GenValidity

import Cursor.Text.Gen ()

import Smos.Cursor.Header

instance GenUnchecked HeaderCursor

instance GenValid HeaderCursor
