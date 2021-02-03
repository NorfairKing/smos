{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Cursor.Report.Timestamps.Gen where

import Cursor.Forest
import Cursor.Forest.Gen ()
import Cursor.Text.Gen ()
import Cursor.Tree
import Data.GenValidity
import Data.GenValidity.Path ()
import qualified Data.Map as M
import Lens.Micro
import Smos.Cursor.Report.Timestamps
import Smos.Data
import Smos.Data.Gen ()
import Test.QuickCheck

instance GenValid TimestampsReportCursor where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = makeTimestampsReportCursor <$> genValid

instance GenValid TimestampsEntryCursor where
  genValid = do
    name <- genValid
    timestamp <- genValid
    timestamps <- M.fromList . ((name, timestamp) :) <$> genValid
    let modEntry e = e {entryTimestamps = timestamps}
    fc <- genValid
    let fc' =
          fc & forestCursorSelectedTreeL . treeCursorCurrentL
            %~ modEntry
    (makeTimestampsEntryCursor <$> genValid <*> pure fc') >>= elements

  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
