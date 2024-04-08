module Smos.Report.Tags where

import Data.Map (Map)
import qualified Data.Set as S
import Smos.Data
import Smos.Report.Stats

newtype TagsReport = TagsReport
  { tagsReportMap :: Map Tag Int
  }

makeTagsReport :: [Entry] -> TagsReport
makeTagsReport e = TagsReport {tagsReportMap = getCount $ concatMap (S.toList . entryTags) e}
