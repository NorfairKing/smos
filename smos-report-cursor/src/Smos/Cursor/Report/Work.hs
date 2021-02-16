{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Cursor.Report.Work where

import Control.DeepSeq
import Data.Maybe
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics
import Lens.Micro
import Smos.Cursor.Report.Entry
import Smos.Cursor.Report.Stuck
import Smos.Cursor.Report.Timestamps
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
  { workReportCursorNextBeginCursor :: !(Maybe (EntryReportEntryCursor (TimestampName, Timestamp))),
    workReportCursorDeadlinesCursor :: !TimestampsReportCursor,
    workReportCursorOverdueWaiting :: !WaitingReportCursor,
    workReportCursorOverdueStuck :: !StuckReportCursor,
    workReportCursorResultEntries :: !(EntryReportCursor ()),
    workReportCursorSelection :: !WorkReportCursorSelection
  }
  deriving (Show, Eq, Generic)

instance Validity WorkReportCursor where
  validate wrc@WorkReportCursor {..} =
    mconcat
      [ genericValidate wrc,
        declare "Anything that is selected, exists" $
          case workReportCursorSelection of
            NextBeginSelected -> isJust workReportCursorNextBeginCursor
            DeadlinesSelected -> isJust $ entryReportCursorSelectedEntryReportEntryCursors $ timestampsReportCursorEntryReportCursor workReportCursorDeadlinesCursor
            WaitingSelected -> isJust $ entryReportCursorSelectedEntryReportEntryCursors $ waitingReportCursorEntryReportCursor workReportCursorOverdueWaiting
            StuckSelected -> isJust $ stuckReportCursorNonEmptyCursor workReportCursorOverdueStuck
            ResultsSelected -> True -- Results can be empty, otherwise an empty report is not valid.
      ]

instance NFData WorkReportCursor

emptyWorkReportCursor :: WorkReportCursor
emptyWorkReportCursor =
  WorkReportCursor
    { workReportCursorNextBeginCursor = Nothing,
      workReportCursorDeadlinesCursor = emptyTimestampsReportCursor,
      workReportCursorOverdueWaiting = emptyWaitingReportCursor,
      workReportCursorOverdueStuck = emptyStuckReportCursor,
      workReportCursorResultEntries = emptyEntryReportCursor,
      workReportCursorSelection = ResultsSelected
    }

intermediateWorkReportToWorkReportCursor :: IntermediateWorkReport -> WorkReportCursor
intermediateWorkReportToWorkReportCursor IntermediateWorkReport {..} =
  let workReportCursorNextBeginCursor = (\(rf, fc, tsn, ts) -> makeEntryReportEntryCursor rf fc (tsn, ts)) <$> intermediateWorkReportNextBegin
      workReportCursorDeadlinesCursor = finaliseTimestampsReportCursor $ flip map intermediateWorkReportAgendaEntries $ \(rf, fc, tsn, ts) -> makeEntryReportEntryCursor rf fc (TimestampsEntryCursor tsn ts)
      workReportCursorSelection = ResultsSelected
      workReportCursorOverdueWaiting = finaliseWaitingReportCursor $ flip map intermediateWorkReportOverdueWaiting $ \(rf, fc, utct) -> makeEntryReportEntryCursor rf fc utct
      workReportCursorOverdueStuck = makeStuckReportCursor intermediateWorkReportOverdueStuck
      workReportCursorResultEntries = makeEntryReportCursor $ flip map intermediateWorkReportResultEntries $ \(rf, fc) -> makeEntryReportEntryCursor rf fc ()
   in WorkReportCursor {..}

data WorkReportCursorSelection
  = NextBeginSelected
  | DeadlinesSelected
  | WaitingSelected
  | StuckSelected
  | ResultsSelected
  deriving (Show, Eq, Generic)

instance Validity WorkReportCursorSelection

instance NFData WorkReportCursorSelection

workReportCursorSelectionL :: Lens' WorkReportCursor WorkReportCursorSelection
workReportCursorSelectionL = lens workReportCursorSelection $ \wrc s -> wrc {workReportCursorSelection = s}

workReportCursorDeadlinesL :: Lens' WorkReportCursor TimestampsReportCursor
workReportCursorDeadlinesL = lens workReportCursorDeadlinesCursor $ \wrc rc -> wrc {workReportCursorDeadlinesCursor = rc}

workReportCursorResultEntriesL :: Lens' WorkReportCursor (EntryReportCursor ())
workReportCursorResultEntriesL = lens workReportCursorResultEntries $ \wrc rc -> wrc {workReportCursorResultEntries = rc}

workReportCursorOverdueWaitingL :: Lens' WorkReportCursor WaitingReportCursor
workReportCursorOverdueWaitingL = lens workReportCursorOverdueWaiting $ \wrc rc -> wrc {workReportCursorOverdueWaiting = rc}

workReportCursorOverdueStuckL :: Lens' WorkReportCursor StuckReportCursor
workReportCursorOverdueStuckL = lens workReportCursorOverdueStuck $ \wrc rc -> wrc {workReportCursorOverdueStuck = rc}

workReportNextBeginEmpty :: WorkReportCursor -> Bool
workReportNextBeginEmpty = isNothing . workReportCursorNextBeginCursor

workReportDeadlinesEmpty :: WorkReportCursor -> Bool
workReportDeadlinesEmpty = isNothing . entryReportCursorSelectedEntryReportEntryCursors . timestampsReportCursorEntryReportCursor . workReportCursorDeadlinesCursor

workReportOverdueWaitingEmpty :: WorkReportCursor -> Bool
workReportOverdueWaitingEmpty = isNothing . entryReportCursorSelectedEntryReportEntryCursors . waitingReportCursorEntryReportCursor . workReportCursorOverdueWaiting

workReportOverdueStuckEmpty :: WorkReportCursor -> Bool
workReportOverdueStuckEmpty = isNothing . stuckReportCursorNonEmptyCursor . workReportCursorOverdueStuck

workReportCursorNext :: WorkReportCursor -> Maybe WorkReportCursor
workReportCursorNext wrc = case workReportCursorSelection wrc of
  NextBeginSelected ->
    let wrc' = wrc {workReportCursorSelection = DeadlinesSelected}
     in if workReportDeadlinesEmpty wrc'
          then workReportCursorNext wrc' -- If there are no deadlines, keep going.
          else Just wrc'
  DeadlinesSelected -> case workReportCursorDeadlinesL timestampsReportCursorNext wrc of
    Just wrc' -> Just wrc'
    Nothing ->
      let wrc' = wrc {workReportCursorSelection = WaitingSelected}
       in if workReportOverdueWaitingEmpty wrc'
            then workReportCursorNext wrc' -- If there were no waiting entries, keep going.
            else Just wrc'
  WaitingSelected -> case workReportCursorOverdueWaitingL waitingReportCursorNext wrc of
    Just wrc' -> Just wrc'
    Nothing ->
      let wrc' = wrc {workReportCursorSelection = StuckSelected}
       in if workReportOverdueStuckEmpty wrc'
            then workReportCursorNext wrc' -- If there are no stuck projects, keep going
            else Just wrc'
  StuckSelected -> case workReportCursorOverdueStuckL stuckReportCursorNext wrc of
    Just wrc' -> Just wrc'
    Nothing -> Just $ wrc {workReportCursorSelection = ResultsSelected} -- Even if there are no results, we stay in the results
  ResultsSelected -> workReportCursorResultEntriesL entryReportCursorNext wrc

workReportCursorPrev :: WorkReportCursor -> Maybe WorkReportCursor
workReportCursorPrev wrc = case workReportCursorSelection wrc of
  NextBeginSelected -> Nothing
  DeadlinesSelected -> case workReportCursorDeadlinesL timestampsReportCursorPrev wrc of
    Just wrc' -> Just wrc'
    Nothing ->
      let wrc' = wrc {workReportCursorSelection = NextBeginSelected}
       in if workReportNextBeginEmpty wrc'
            then Nothing -- If there is no next begin, fail here.
            else Just wrc'
  WaitingSelected -> case workReportCursorOverdueWaitingL waitingReportCursorPrev wrc of
    Just wrc' -> Just wrc'
    Nothing ->
      let wrc' = wrc {workReportCursorSelection = DeadlinesSelected}
       in if workReportDeadlinesEmpty wrc'
            then workReportCursorPrev wrc' -- If there are no deadlines, keep looking up
            else Just wrc'
  StuckSelected -> case workReportCursorOverdueStuckL stuckReportCursorPrev wrc of
    Just wrc' -> Just wrc'
    Nothing ->
      let wrc' = wrc {workReportCursorSelection = WaitingSelected}
       in if workReportOverdueWaitingEmpty wrc'
            then workReportCursorPrev wrc' -- If there are no waiting entries, keep looking up
            else Just wrc'
  ResultsSelected -> case workReportCursorResultEntriesL entryReportCursorPrev wrc of
    Just wrc' -> Just wrc'
    Nothing ->
      let wrc' = wrc {workReportCursorSelection = StuckSelected}
       in if workReportOverdueStuckEmpty wrc'
            then workReportCursorPrev wrc' -- If there are no stuck projects, keep looking up
            else Just wrc'

workReportCursorFirst :: WorkReportCursor -> WorkReportCursor
workReportCursorFirst = id -- TODO: write tests before  implementing this

-- wrc
--   & workReportCursorSelectionL .~ NextBeginSelected
--   & workReportCursorResultEntriesL %~ entryReportCursorFirst
--   & workReportCursorOverdueWaitingL %~ waitingReportCursorFirst
--   & workReportCursorOverdueStuckL %~ stuckReportCursorFirst

workReportCursorLast :: WorkReportCursor -> WorkReportCursor
workReportCursorLast wrc =
  wrc
    & workReportCursorSelectionL .~ ResultsSelected
    & workReportCursorResultEntriesL %~ entryReportCursorLast
    & workReportCursorOverdueWaitingL %~ waitingReportCursorLast
    & workReportCursorOverdueStuckL %~ stuckReportCursorLast

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
