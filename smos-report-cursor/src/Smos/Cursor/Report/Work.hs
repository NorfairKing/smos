{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Cursor.Report.Work where

import Data.Validity
import Data.Validity.Path ()
import GHC.Generics
import Lens.Micro
import Smos.Cursor.Report.Entry
import Smos.Cursor.Report.Stuck
import Smos.Cursor.Report.Waiting
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
    workReportCursorOverdueWaiting :: !WaitingReportCursor,
    workReportCursorSelection :: !WorkReportCursorSelection
  }
  deriving (Show, Eq, Generic)

instance Validity WorkReportCursor

intermediateWorkReportToWorkReportCursor :: IntermediateWorkReport -> WorkReportCursor
intermediateWorkReportToWorkReportCursor IntermediateWorkReport {..} =
  let workReportCursorNextBeginCursor = (\(rf, fc, tsn, ts) -> makeEntryReportEntryCursor rf fc (tsn, ts)) <$> intermediateWorkReportNextBegin
      workReportCursorResultEntries = makeEntryReportCursor $ flip map intermediateWorkReportResultEntries $ \(rf, fc) -> makeEntryReportEntryCursor rf fc ()
      workReportCursorSelection = NextBeginSelected
      workReportCursorOverdueWaiting = finaliseWaitingReportCursor $ flip map intermediateWorkReportOverdueWaiting $ \(rf, fc, utct) -> makeEntryReportEntryCursor rf fc utct
   in WorkReportCursor {..}

data WorkReportCursorSelection
  = NextBeginSelected
  | ResultsSelected
  | WaitingSelected
  deriving (Show, Eq, Generic)

instance Validity WorkReportCursorSelection

workReportCursorResultEntriesL :: Lens' WorkReportCursor (EntryReportCursor ())
workReportCursorResultEntriesL = lens workReportCursorResultEntries $ \wrc rc -> wrc {workReportCursorResultEntries = rc}

workReportCursorSelectionL :: Lens' WorkReportCursor WorkReportCursorSelection
workReportCursorSelectionL = lens workReportCursorSelection $ \wrc s -> wrc {workReportCursorSelection = s}

workReportCursorOverdueWaitingL :: Lens' WorkReportCursor WaitingReportCursor
workReportCursorOverdueWaitingL = lens workReportCursorOverdueWaiting $ \wrc rc -> wrc {workReportCursorOverdueWaiting = rc}

workReportCursorNext :: WorkReportCursor -> Maybe WorkReportCursor
workReportCursorNext wrc = case workReportCursorSelection wrc of
  NextBeginSelected -> Just $ wrc & workReportCursorSelectionL .~ ResultsSelected
  ResultsSelected -> case workReportCursorResultEntriesL entryReportCursorNext wrc of
    Nothing -> Just $ wrc & workReportCursorSelectionL .~ WaitingSelected
    Just wrc' -> Just wrc'
  WaitingSelected -> workReportCursorOverdueWaitingL waitingReportCursorNext wrc

workReportCursorPrev :: WorkReportCursor -> Maybe WorkReportCursor
workReportCursorPrev wrc = case workReportCursorSelection wrc of
  NextBeginSelected -> Nothing
  ResultsSelected -> case workReportCursorResultEntriesL entryReportCursorPrev wrc of
    Nothing -> Just $ wrc & workReportCursorSelectionL .~ NextBeginSelected
    Just wrc' -> Just wrc'
  WaitingSelected -> case workReportCursorOverdueWaitingL waitingReportCursorPrev wrc of
    Nothing -> Just $ wrc & workReportCursorSelectionL .~ ResultsSelected
    Just wrc' -> Just wrc'

workReportCursorFirst :: WorkReportCursor -> WorkReportCursor
workReportCursorFirst wrc =
  wrc
    & workReportCursorSelectionL .~ NextBeginSelected
    & workReportCursorResultEntriesL %~ entryReportCursorFirst
    & workReportCursorOverdueWaitingL %~ waitingReportCursorFirst

workReportCursorLast :: WorkReportCursor -> WorkReportCursor
workReportCursorLast wrc =
  wrc
    & workReportCursorSelectionL .~ WaitingSelected
    & workReportCursorResultEntriesL %~ entryReportCursorLast
    & workReportCursorOverdueWaitingL %~ waitingReportCursorLast

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
