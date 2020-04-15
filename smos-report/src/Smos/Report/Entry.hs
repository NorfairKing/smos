{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Entry where

import Cursor.Simple.Forest
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Validity
import GHC.Generics (Generic)
import Smos.Data
import Smos.Report.Path
import Smos.Report.Projection

data EntryReport
  = EntryReport
      { entryReportHeaders :: NonEmpty Projection,
        entryReportCells :: [NonEmpty Projectee]
      }
  deriving (Show, Eq, Generic)

instance Validity EntryReport

makeEntryReport :: NonEmpty Projection -> [(RootedPath, ForestCursor Entry)] -> EntryReport
makeEntryReport entryReportHeaders tups =
  let entryReportCells =
        flip map tups $ \(rp, e) ->
          flip NE.map entryReportHeaders $ \projection -> performProjection projection rp e
   in EntryReport {..}
