{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Report.Next.Gen where

import Data.GenValidity
import Data.GenValidity.Path ()

import Smos.Data.Gen ()

import Cursor.Forest.Gen ()

import Smos.Cursor.Report.Next

instance GenUnchecked NextActionEntryCursor

instance GenValid NextActionEntryCursor where
    genValid = genValidStructurally
