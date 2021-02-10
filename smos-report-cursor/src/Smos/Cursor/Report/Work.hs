{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Cursor.Report.Work where

import Data.Validity
import Data.Validity.Path ()
import GHC.Generics
import Lens.Micro
import Smos.Cursor.Report.Entry
import Smos.Data
import Smos.Report.Archive
import Smos.Report.Config
import Smos.Report.ShouldPrint
import Smos.Report.Streaming
import Smos.Report.Work

produceWorkReportCursor :: HideArchive -> ShouldPrint -> DirectoryConfig -> WorkReportContext -> IO WorkReportCursor
produceWorkReportCursor ha sp dc wrc = produceReport ha sp dc $ intermediateWorkReportToWorkReportCursor <$> intermediateWorkReportConduit wrc

data WorkReportCursor = WorkReportCursor
  { workReportCursorNextBeginCursor :: Maybe (EntryReportEntryCursor (TimestampName, Timestamp)),
    workReportCursorResultEntries :: !(EntryReportCursor ()),
    workReportCursorSelection :: !WorkReportCursorSelection
  }
  deriving (Show, Eq, Generic)

instance Validity WorkReportCursor

intermediateWorkReportToWorkReportCursor :: IntermediateWorkReport -> WorkReportCursor
intermediateWorkReportToWorkReportCursor IntermediateWorkReport {..} =
  let workReportCursorNextBeginCursor = (\(rf, fc, tsn, ts) -> makeEntryReportEntryCursor rf fc (tsn, ts)) <$> intermediateWorkReportNextBegin
      workReportCursorResultEntries = makeEntryReportCursor $ flip map intermediateWorkReportResultEntries $ \(rf, fc) -> makeEntryReportEntryCursor rf fc ()
      workReportCursorSelection = NextBeginSelected
   in WorkReportCursor {..}

data WorkReportCursorSelection = NextBeginSelected | ResultsSelected
  deriving (Show, Eq, Generic)

instance Validity WorkReportCursorSelection

workReportCursorResultEntriesL :: Lens' WorkReportCursor (EntryReportCursor ())
workReportCursorResultEntriesL = lens workReportCursorResultEntries $ \wrc rc -> wrc {workReportCursorResultEntries = rc}

workReportCursorResultSelectionL :: Lens' WorkReportCursor WorkReportCursorSelection
workReportCursorResultSelectionL = lens workReportCursorSelection $ \wrc s -> wrc {workReportCursorSelection = s}

workReportCursorNext :: WorkReportCursor -> Maybe WorkReportCursor
workReportCursorNext wrc = case workReportCursorSelection wrc of
  NextBeginSelected -> Just $ wrc & workReportCursorResultSelectionL .~ ResultsSelected
  ResultsSelected -> workReportCursorResultEntriesL entryReportCursorNext wrc

workReportCursorPrev :: WorkReportCursor -> Maybe WorkReportCursor
workReportCursorPrev wrc = case workReportCursorSelection wrc of
  NextBeginSelected -> Nothing
  ResultsSelected -> case workReportCursorResultEntriesL entryReportCursorPrev wrc of
    Nothing -> Just $ wrc & workReportCursorResultSelectionL .~ NextBeginSelected
    Just wrc' -> Just wrc'

workReportCursorFirst :: WorkReportCursor -> WorkReportCursor
workReportCursorFirst wrc =
  wrc
    & workReportCursorResultSelectionL .~ NextBeginSelected
    & workReportCursorResultEntriesL %~ entryReportCursorFirst

workReportCursorLast :: WorkReportCursor -> WorkReportCursor
workReportCursorLast wrc =
  wrc
    & workReportCursorResultSelectionL .~ ResultsSelected
    & workReportCursorResultEntriesL %~ entryReportCursorLast

workReportCursorSelectReport :: WorkReportCursor -> Maybe WorkReportCursor
workReportCursorSelectReport = workReportCursorResultEntriesL entryReportCursorSelectReport

workReportCursorSelectFilter :: WorkReportCursor -> Maybe WorkReportCursor
workReportCursorSelectFilter = workReportCursorResultEntriesL entryReportCursorSelectFilter

workReportCursorInsert :: Char -> WorkReportCursor -> Maybe WorkReportCursor
workReportCursorInsert c = workReportCursorResultEntriesL $ entryReportCursorInsert c

workReportCursorAppend :: Char -> WorkReportCursor -> Maybe WorkReportCursor
workReportCursorAppend c = workReportCursorResultEntriesL $ entryReportCursorAppend c

workReportCursorRemove :: WorkReportCursor -> Maybe WorkReportCursor
workReportCursorRemove = workReportCursorResultEntriesL entryReportCursorRemove

workReportCursorDelete :: WorkReportCursor -> Maybe WorkReportCursor
workReportCursorDelete = workReportCursorResultEntriesL entryReportCursorDelete
