{-# LANGUAGE DeriveGeneric #-}

module Smos.Query.Clock.Types where

import Data.Text (Text)
import Data.Time
import GHC.Generics (Generic)
import Path
import Smos.Data

data ClockTableRow
  = BlockTitleRow Text
  | FileRow (Path Rel File) NominalDiffTime
  | EntryRow
      Int -- Levels deep (0 is the top level)
      Header
      NominalDiffTime -- Time on this individual header
      NominalDiffTime -- Total time for this subtree
  | BlockTotalRow NominalDiffTime
  | AllTotalRow NominalDiffTime
  deriving (Show, Eq, Generic)
