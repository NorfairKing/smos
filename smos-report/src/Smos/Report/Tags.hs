{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Tags where

import GHC.Generics (Generic)

import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Data.Time
import Data.Tree
import Path

import Smos.Data

import Smos.Report.Path
import Smos.Report.Period
import Smos.Report.Stats

newtype TagsReport =
  TagsReport
    { tagsReportMap :: Map Tag Int
    }
  deriving (Show, Eq, Generic)

instance Semigroup TagsReport where
  tr1 <> tr2 = TagsReport {tagsReportMap = M.unionWith (+) (tagsReportMap tr1) (tagsReportMap tr2)}

instance Monoid TagsReport where
  mempty = TagsReport {tagsReportMap = M.empty}

makeTagsReport :: [Entry] -> TagsReport
makeTagsReport e = TagsReport {tagsReportMap = getCount $ concatMap entryTags e}
