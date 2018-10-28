{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Tags.Gen where

import Data.GenValidity

import Cursor.List.NonEmpty.Gen ()
import Cursor.Text.Gen ()

import Smos.Data.Gen ()

import Smos.Cursor.Tag.Gen ()
import Smos.Cursor.Tags

instance GenUnchecked TagsCursor

instance GenValid TagsCursor
