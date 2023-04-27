{-# LANGUAGE DeriveGeneric #-}

module Smos.Report.Tags where

import Data.Map (Map)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Smos.Data
import Smos.Report.Stats

newtype TagsReport = TagsReport
  { tagsReportMap :: Map Tag Int
  }
  deriving (Show, Eq, Generic)

makeTagsReport :: [Entry] -> TagsReport
makeTagsReport e = TagsReport {tagsReportMap = getCount $ concatMap (S.toList . entryTags) e}
