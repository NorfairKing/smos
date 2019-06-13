{-# LANGUAGE DeriveGeneric #-}

module Smos.Query.Clock.Types where

import GHC.Generics (Generic)

import Data.Text (Text)
import Data.Time

import Smos.Data

import Smos.Report.Path

data ClockTableRow
  = BlockTitleRow Text
  | FileRow RootedPath NominalDiffTime
  | EntryRow
      Int -- Levels deep (0 is the top level)
      Header
      NominalDiffTime -- Time on this individual header
      NominalDiffTime -- Total time for this subtree
  | BlockTotalRow NominalDiffTime
  | AllTotalRow NominalDiffTime
  deriving (Show, Eq, Generic)
