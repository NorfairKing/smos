{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Smos.Cursor.Report.Work where

import Control.DeepSeq
import Cursor.Map.KeyValue (keyValueCursorTraverseKeyCase)
import Cursor.Simple.Map
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
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
import Smos.Report.Filter
import Smos.Report.ShouldPrint
import Smos.Report.Streaming
import Smos.Report.Work

produceWorkReportCursor :: HideArchive -> ShouldPrint -> DirectoryConfig -> WorkReportContext -> IO WorkReportCursor
produceWorkReportCursor ha sp dc wrc = produceReport ha sp dc $ intermediateWorkReportToWorkReportCursor <$> intermediateWorkReportConduit wrc

data WorkReportCursor = WorkReportCursor
  { workReportCursorNextBeginCursor :: !(Maybe (EntryReportEntryCursor (TimestampName, Timestamp))),
    workReportCursorEntriesWithoutContext :: !(EntryReportCursor ()),
    workReportCursorCheckViolations :: !(Maybe (MapCursor EntryFilterRel (EntryReportCursor ()))),
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
            NextBeginSelected -> not $ workReportNextBeginEmpty wrc
            WithoutContextSelected -> not $ workReportWithoutContextEmpty wrc
            CheckViolationsSelected -> not $ workReportCheckViolationsEmpty wrc
            DeadlinesSelected -> not $ workReportDeadlinesEmpty wrc
            WaitingSelected -> not $ workReportOverdueWaitingEmpty wrc
            StuckSelected -> not $ workReportOverdueStuckEmpty wrc
            ResultsSelected -> True -- Results can be empty, otherwise an empty report is not valid.
            -- TODO add this constraint
            -- declare "The work report cursor determines the selections of unselected parts" $ case workReportCursorSelection of
            -- e.g. when next begin is selected then everything else needs to have its first element selected.
      ]

instance NFData WorkReportCursor

emptyWorkReportCursor :: WorkReportCursor
emptyWorkReportCursor =
  WorkReportCursor
    { workReportCursorNextBeginCursor = Nothing,
      workReportCursorEntriesWithoutContext = emptyEntryReportCursor,
      workReportCursorCheckViolations = Nothing,
      workReportCursorDeadlinesCursor = emptyTimestampsReportCursor,
      workReportCursorOverdueWaiting = emptyWaitingReportCursor,
      workReportCursorOverdueStuck = emptyStuckReportCursor,
      workReportCursorResultEntries = emptyEntryReportCursor,
      workReportCursorSelection = ResultsSelected
    }

intermediateWorkReportToWorkReportCursor :: IntermediateWorkReport -> WorkReportCursor
intermediateWorkReportToWorkReportCursor IntermediateWorkReport {..} =
  let IntermediateWorkReport _ _ _ _ _ _ _ = undefined
      workReportCursorNextBeginCursor = (\(rf, fc, tsn, ts) -> makeEntryReportEntryCursor rf fc (tsn, ts)) <$> intermediateWorkReportNextBegin
      workReportCursorEntriesWithoutContext = makeEntryReportCursor $ flip map intermediateWorkReportEntriesWithoutContext $ \(rf, fc) -> makeEntryReportEntryCursor rf fc ()
      workReportCursorCheckViolations =
        makeMapCursor
          . NE.map
            (\(erf, tups) -> (erf, makeEntryReportCursor $ flip map tups $ \(rf, fc) -> makeEntryReportEntryCursor rf fc ()))
          <$> NE.nonEmpty (M.toList intermediateWorkReportCheckViolations)
      workReportCursorDeadlinesCursor = finaliseTimestampsReportCursor $ flip map intermediateWorkReportAgendaEntries $ \(rf, fc, tsn, ts) -> makeEntryReportEntryCursor rf fc (TimestampsEntryCursor tsn ts)
      workReportCursorOverdueWaiting = finaliseWaitingReportCursor $ flip map intermediateWorkReportOverdueWaiting $ \(rf, fc, utct) -> makeEntryReportEntryCursor rf fc utct
      workReportCursorOverdueStuck = makeStuckReportCursor intermediateWorkReportOverdueStuck
      workReportCursorResultEntries = makeEntryReportCursor $ flip map intermediateWorkReportResultEntries $ \(rf, fc) -> makeEntryReportEntryCursor rf fc ()
      workReportCursorSelection = NextBeginSelected
      wrc = WorkReportCursor {..}
   in fromMaybe wrc $ workReportCursorNext wrc -- should not fail.

-- The order of these constructors matters for shrinking
data WorkReportCursorSelection
  = ResultsSelected
  | WithoutContextSelected
  | CheckViolationsSelected
  | StuckSelected
  | WaitingSelected
  | DeadlinesSelected
  | NextBeginSelected
  deriving (Show, Eq, Generic)

instance Validity WorkReportCursorSelection

instance NFData WorkReportCursorSelection

workReportCursorSelectionL :: Lens' WorkReportCursor WorkReportCursorSelection
workReportCursorSelectionL = lens workReportCursorSelection $ \wrc s -> wrc {workReportCursorSelection = s}

workReportCursorCheckViolationsL :: Lens' WorkReportCursor (Maybe (MapCursor EntryFilterRel (EntryReportCursor ())))
workReportCursorCheckViolationsL = lens workReportCursorCheckViolations $ \wrc rc -> wrc {workReportCursorCheckViolations = rc}

workReportCursorEntriesWithoutContextL :: Lens' WorkReportCursor (EntryReportCursor ())
workReportCursorEntriesWithoutContextL = lens workReportCursorEntriesWithoutContext $ \wrc rc -> wrc {workReportCursorEntriesWithoutContext = rc}

workReportCursorDeadlinesL :: Lens' WorkReportCursor TimestampsReportCursor
workReportCursorDeadlinesL = lens workReportCursorDeadlinesCursor $ \wrc rc -> wrc {workReportCursorDeadlinesCursor = rc}

workReportCursorOverdueWaitingL :: Lens' WorkReportCursor WaitingReportCursor
workReportCursorOverdueWaitingL = lens workReportCursorOverdueWaiting $ \wrc rc -> wrc {workReportCursorOverdueWaiting = rc}

workReportCursorOverdueStuckL :: Lens' WorkReportCursor StuckReportCursor
workReportCursorOverdueStuckL = lens workReportCursorOverdueStuck $ \wrc rc -> wrc {workReportCursorOverdueStuck = rc}

workReportCursorResultEntriesL :: Lens' WorkReportCursor (EntryReportCursor ())
workReportCursorResultEntriesL = lens workReportCursorResultEntries $ \wrc rc -> wrc {workReportCursorResultEntries = rc}

workReportNextBeginEmpty :: WorkReportCursor -> Bool
workReportNextBeginEmpty = isNothing . workReportCursorNextBeginCursor

workReportWithoutContextEmpty :: WorkReportCursor -> Bool
workReportWithoutContextEmpty = isNothing . entryReportCursorSelectedEntryReportEntryCursors . workReportCursorEntriesWithoutContext

workReportCheckViolationsEmpty :: WorkReportCursor -> Bool
workReportCheckViolationsEmpty = isNothing . workReportCursorCheckViolations

workReportDeadlinesEmpty :: WorkReportCursor -> Bool
workReportDeadlinesEmpty = isNothing . entryReportCursorSelectedEntryReportEntryCursors . timestampsReportCursorEntryReportCursor . workReportCursorDeadlinesCursor

workReportOverdueWaitingEmpty :: WorkReportCursor -> Bool
workReportOverdueWaitingEmpty = isNothing . entryReportCursorSelectedEntryReportEntryCursors . waitingReportCursorEntryReportCursor . workReportCursorOverdueWaiting

workReportOverdueStuckEmpty :: WorkReportCursor -> Bool
workReportOverdueStuckEmpty = isNothing . stuckReportCursorNonEmptyCursor . workReportCursorOverdueStuck

workReportResultsEmpty :: WorkReportCursor -> Bool
workReportResultsEmpty = isNothing . entryReportCursorSelectedEntryReportEntryCursors . workReportCursorResultEntries

checkViolationsNext ::
  Maybe (MapCursor EntryFilterRel (EntryReportCursor ())) ->
  Maybe (Maybe (MapCursor EntryFilterRel (EntryReportCursor ())))
checkViolationsNext mmc = do
  mc <- mmc
  let kvcNext :: EntryFilterRel -> EntryReportCursor () -> Maybe (EntryFilterRel, EntryReportCursor ())
      kvcNext f erc = (,) f <$> entryReportCursorNext erc
  case mapCursorElemL (keyValueCursorTraverseKeyCase kvcNext) mc of
    Just mc' -> pure $ Just mc'
    Nothing -> case mapCursorSelectNext mc of
      Nothing -> Nothing
      Just mc' -> Just (Just mc')

checkViolationsPrev ::
  Maybe (MapCursor EntryFilterRel (EntryReportCursor ())) ->
  Maybe (Maybe (MapCursor EntryFilterRel (EntryReportCursor ())))
checkViolationsPrev mmc = do
  mc <- mmc
  let kvcPrev :: EntryFilterRel -> EntryReportCursor () -> Maybe (EntryFilterRel, EntryReportCursor ())
      kvcPrev f erc = (,) f <$> entryReportCursorPrev erc
  case mapCursorElemL (keyValueCursorTraverseKeyCase kvcPrev) mc of
    Just mc' -> pure $ Just mc'
    Nothing -> case mapCursorSelectPrev mc of
      Nothing -> Nothing
      Just mc' -> Just (Just mc')

checkViolationsFirst ::
  Maybe (MapCursor EntryFilterRel (EntryReportCursor ())) ->
  Maybe (MapCursor EntryFilterRel (EntryReportCursor ()))
checkViolationsFirst = fmap $ \mc -> mapCursorSelectFirst (mapMapCursor id entryReportCursorFirst (mapCursorSelectKey mc))

checkViolationsLast ::
  Maybe (MapCursor EntryFilterRel (EntryReportCursor ())) ->
  Maybe (MapCursor EntryFilterRel (EntryReportCursor ()))
checkViolationsLast = fmap $ \mc -> mapCursorSelectLast (mapMapCursor id entryReportCursorLast (mapCursorSelectKey mc))

workReportCursorNext :: WorkReportCursor -> Maybe WorkReportCursor
workReportCursorNext wrc = case workReportCursorSelection wrc of
  NextBeginSelected ->
    let wrc' = wrc {workReportCursorSelection = WithoutContextSelected}
     in if workReportWithoutContextEmpty wrc'
          then workReportCursorNext wrc' -- If there are no entries without context, keep going.
          else Just wrc'
  WithoutContextSelected -> case workReportCursorEntriesWithoutContextL entryReportCursorNext wrc of
    Just wrc' -> Just wrc'
    Nothing ->
      let wrc' = wrc {workReportCursorSelection = CheckViolationsSelected}
       in if workReportCheckViolationsEmpty wrc'
            then workReportCursorNext wrc' -- If there were deadlines entries, keep going.
            else Just wrc'
  CheckViolationsSelected -> case workReportCursorCheckViolationsL checkViolationsNext wrc of
    Just wrc' -> Just wrc'
    Nothing ->
      let wrc' = wrc {workReportCursorSelection = DeadlinesSelected}
       in if workReportDeadlinesEmpty wrc'
            then workReportCursorNext wrc' -- If there are no entries without context, keep going.
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
  WithoutContextSelected -> case workReportCursorEntriesWithoutContextL entryReportCursorPrev wrc of
    Just wrc' -> Just wrc'
    Nothing ->
      let wrc' = wrc {workReportCursorSelection = NextBeginSelected}
       in if workReportNextBeginEmpty wrc'
            then workReportCursorPrev wrc' -- If there are no next begin, keep going.
            else Just wrc'
  CheckViolationsSelected -> case workReportCursorCheckViolationsL checkViolationsPrev wrc of
    Just wrc' -> Just wrc'
    Nothing ->
      let wrc' = wrc {workReportCursorSelection = WithoutContextSelected}
       in if workReportWithoutContextEmpty wrc'
            then Nothing -- If there are no entries without context, keep going.
            else Just wrc'
  DeadlinesSelected -> case workReportCursorDeadlinesL timestampsReportCursorPrev wrc of
    Just wrc' -> Just wrc'
    Nothing ->
      let wrc' = wrc {workReportCursorSelection = CheckViolationsSelected}
       in if workReportCheckViolationsEmpty wrc'
            then workReportCursorPrev wrc' -- If there are no check violations, keep going.
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
workReportCursorFirst wrc =
  let wrc' =
        wrc
          & workReportCursorSelectionL .~ NextBeginSelected
          & workReportCursorCheckViolationsL %~ checkViolationsFirst
          & workReportCursorEntriesWithoutContextL %~ entryReportCursorFirst
          & workReportCursorDeadlinesL %~ timestampsReportCursorFirst
          & workReportCursorOverdueWaitingL %~ waitingReportCursorFirst
          & workReportCursorOverdueStuckL %~ stuckReportCursorFirst
          & workReportCursorResultEntriesL %~ entryReportCursorFirst
   in case workReportCursorNext wrc' of
        Nothing -> wrc' -- Should not happen.
        Just wrc'' ->
          fromMaybe wrc'' $ -- If there are only results
            workReportCursorPrev wrc''

workReportCursorLast :: WorkReportCursor -> WorkReportCursor
workReportCursorLast wrc =
  wrc
    & workReportCursorSelectionL .~ ResultsSelected
    & workReportCursorCheckViolationsL %~ checkViolationsLast
    & workReportCursorEntriesWithoutContextL %~ entryReportCursorLast
    & workReportCursorDeadlinesL %~ timestampsReportCursorLast
    & workReportCursorOverdueWaitingL %~ waitingReportCursorLast
    & workReportCursorOverdueStuckL %~ stuckReportCursorLast
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
