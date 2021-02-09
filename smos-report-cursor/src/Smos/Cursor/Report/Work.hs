{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Cursor.Report.Work where

import Conduit
import Cursor.Simple.List.NonEmpty
import qualified Data.Conduit.Combinators as C
import qualified Data.List.NonEmpty as NE
import Data.Time
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics
import Lens.Micro
import Path
import Smos.Cursor.Report.Entry
import Smos.Data
import Smos.Report.Archive
import Smos.Report.Config
import Smos.Report.ShouldPrint
import Smos.Report.Streaming
import Smos.Report.Work

produceWorkReportCursor :: HideArchive -> ShouldPrint -> DirectoryConfig -> IO WorkReportCursor
produceWorkReportCursor = undefined

newtype WorkReportCursor = WorkReportCursor
  { workReportCursorResultEntries :: EntryReportCursor ()
  }
  deriving (Show, Eq, Generic)

instance Validity WorkReportCursor
