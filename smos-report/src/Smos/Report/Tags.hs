{-# LANGUAGE DeriveGeneric #-}

module Smos.Report.Tags where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Generics (Generic)
import Smos.Data
import Smos.Report.Stats

newtype TagsReport = TagsReport
  { tagsReportMap :: Map Tag Int
  }
  deriving (Show, Eq, Generic)

instance Semigroup TagsReport where
  tr1 <> tr2 = TagsReport {tagsReportMap = M.unionWith (+) (tagsReportMap tr1) (tagsReportMap tr2)}

instance Monoid TagsReport where
  mempty = TagsReport {tagsReportMap = M.empty}

makeTagsReport :: [Entry] -> TagsReport
makeTagsReport e = TagsReport {tagsReportMap = getCount $ concatMap (S.toList . entryTags) e}
