{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Work where

import Conduit
import Control.Monad
import Cursor.Simple.Forest
import qualified Data.Conduit.Combinators as C
import Data.Function
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import Data.Time
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics (Generic)
import Path
import Safe
import Smos.Data
import Smos.Report.Agenda
import Smos.Report.Archive
import Smos.Report.Comparison
import Smos.Report.Config
import Smos.Report.Filter
import Smos.Report.ShouldPrint
import Smos.Report.Sorter
import Smos.Report.Streaming
import Smos.Report.Stuck
import Smos.Report.Time
import Smos.Report.Waiting

produceWorkReport :: MonadIO m => HideArchive -> ShouldPrint -> DirectoryConfig -> WorkReportContext -> m WorkReport
produceWorkReport ha sp dc wrc = produceReport ha sp dc $ workReportConduit wrc

workReportConduit :: Monad m => WorkReportContext -> ConduitT (Path Rel File, SmosFile) void m WorkReport
workReportConduit wrc@WorkReportContext {..} =
  finishWorkReport workReportContextNow workReportContextTimeProperty workReportContextTime workReportContextSorter <$> intermediateWorkReportConduit wrc

intermediateWorkReportConduit :: Monad m => WorkReportContext -> ConduitT (Path Rel File, SmosFile) void m IntermediateWorkReport
intermediateWorkReportConduit wrc =
  C.map (uncurry $ makeIntermediateWorkReportForFile wrc) .| accumulateMonoid

data IntermediateWorkReport = IntermediateWorkReport
  { intermediateWorkReportResultEntries :: ![(Path Rel File, ForestCursor Entry)],
    intermediateWorkReportAgendaEntries :: ![(Path Rel File, ForestCursor Entry, TimestampName, Timestamp)],
    intermediateWorkReportNextBegin :: !(Maybe (Path Rel File, ForestCursor Entry, TimestampName, Timestamp)),
    intermediateWorkReportOverdueWaiting :: ![(Path Rel File, ForestCursor Entry, UTCTime, Maybe Time)],
    intermediateWorkReportOverdueStuck :: ![StuckReportEntry],
    intermediateWorkReportEntriesWithoutContext :: ![(Path Rel File, ForestCursor Entry)],
    intermediateWorkReportCheckViolations :: !(Map EntryFilter [(Path Rel File, ForestCursor Entry)])
  }
  deriving (Show, Eq, Generic)

instance Validity IntermediateWorkReport

instance Semigroup IntermediateWorkReport where
  wr1 <> wr2 =
    IntermediateWorkReport
      { intermediateWorkReportResultEntries = intermediateWorkReportResultEntries wr1 <> intermediateWorkReportResultEntries wr2,
        intermediateWorkReportAgendaEntries = intermediateWorkReportAgendaEntries wr1 <> intermediateWorkReportAgendaEntries wr2,
        intermediateWorkReportNextBegin = case (intermediateWorkReportNextBegin wr1, intermediateWorkReportNextBegin wr2) of
          (Nothing, Nothing) -> Nothing
          (Just ae, Nothing) -> Just ae
          (Nothing, Just ae) -> Just ae
          (Just ae1, Just ae2) ->
            Just $
              if ((<=) `on` (timestampLocalTime . fth)) ae1 ae2
                then ae1
                else ae2,
        intermediateWorkReportOverdueWaiting = intermediateWorkReportOverdueWaiting wr1 <> intermediateWorkReportOverdueWaiting wr2,
        intermediateWorkReportOverdueStuck = intermediateWorkReportOverdueStuck wr1 <> intermediateWorkReportOverdueStuck wr2,
        intermediateWorkReportCheckViolations =
          M.unionWith (++) (intermediateWorkReportCheckViolations wr1) (intermediateWorkReportCheckViolations wr2),
        intermediateWorkReportEntriesWithoutContext =
          intermediateWorkReportEntriesWithoutContext wr1 <> intermediateWorkReportEntriesWithoutContext wr2
      }

instance Monoid IntermediateWorkReport where
  mempty =
    IntermediateWorkReport
      { intermediateWorkReportResultEntries = mempty,
        intermediateWorkReportAgendaEntries = [],
        intermediateWorkReportNextBegin = Nothing,
        intermediateWorkReportOverdueWaiting = [],
        intermediateWorkReportOverdueStuck = [],
        intermediateWorkReportEntriesWithoutContext = mempty,
        intermediateWorkReportCheckViolations = M.empty
      }

data WorkReportContext = WorkReportContext
  { workReportContextNow :: !ZonedTime, -- Current time, for computing whether something is overdue
    workReportContextProjectsSubdir :: !(Maybe (Path Rel Dir)), -- Projects, for deciding whether a file is a project
    workReportContextBaseFilter :: !(Maybe EntryFilter), -- Base filter, for filtering out most things and selecting next action entries
    workReportContextCurrentContext :: !(Maybe EntryFilter), -- Filter for the current context
    workReportContextTimeProperty :: !(Maybe PropertyName), -- The property to filter time by, Nothing means no time filtering
    workReportContextTime :: !(Maybe Time), -- The time to filter by, Nothing means don't discriminate on time
    workReportContextAdditionalFilter :: !(Maybe EntryFilter), -- Additional filter, for an extra filter argument
    workReportContextContexts :: !(Map ContextName EntryFilter), -- Map of contexts, for checking whether any entry has no context
    workReportContextChecks :: !(Set EntryFilter), -- Extra checks to perform
    workReportContextSorter :: !(Maybe Sorter), -- How to sort the next action entries, Nothing means no sorting
    workReportContextWaitingThreshold :: !Time, -- When to consider waiting entries 'overdue'
    workReportContextStuckThreshold :: !Time -- When to consider stuck projects 'overdue' (days)
  }
  deriving (Show, Generic)

instance Validity WorkReportContext

makeIntermediateWorkReportForFile :: WorkReportContext -> Path Rel File -> SmosFile -> IntermediateWorkReport
makeIntermediateWorkReportForFile ctx@WorkReportContext {..} rp sf =
  let iwr = foldMap (makeIntermediateWorkReport ctx rp) (allCursors sf)
      mStuckEntry :: Maybe StuckReportEntry
      mStuckEntry = do
        -- To make sure that only projects are considered
        _ <- case workReportContextProjectsSubdir of
          Nothing -> Just ()
          Just psd -> () <$ stripProperPrefix psd rp
        se <- makeStuckReportEntry (zonedTimeZone workReportContextNow) rp sf
        latestChange <- stuckReportEntryLatestChange se
        let diff = diffUTCTime (zonedTimeToUTC workReportContextNow) latestChange
        guard (diff >= timeNominalDiffTime workReportContextStuckThreshold)
        pure se
   in iwr
        { intermediateWorkReportOverdueStuck = maybeToList mStuckEntry
        }

makeIntermediateWorkReport :: WorkReportContext -> Path Rel File -> ForestCursor Entry -> IntermediateWorkReport
makeIntermediateWorkReport WorkReportContext {..} rp fc =
  let match b = [(rp, fc) | b]
      combineFilter :: EntryFilter -> Maybe EntryFilter -> EntryFilter
      combineFilter f = maybe f (FilterAnd f)
      combineMFilter :: Maybe EntryFilter -> Maybe EntryFilter -> Maybe EntryFilter
      combineMFilter mf1 mf2 = case (mf1, mf2) of
        (Nothing, Nothing) -> Nothing
        (Just f1, Nothing) -> Just f1
        (Nothing, Just f2) -> Just f2
        (Just f1, Just f2) -> Just $ FilterAnd f1 f2
      filterWithBase :: EntryFilter -> EntryFilter
      filterWithBase f = combineFilter f workReportContextBaseFilter
      filterMWithBase :: Maybe EntryFilter -> Maybe EntryFilter
      filterMWithBase mf = combineMFilter mf workReportContextBaseFilter
      totalCurrent :: Maybe EntryFilter
      totalCurrent =
        combineMFilter workReportContextCurrentContext $ do
          t <- workReportContextTime
          pn <- workReportContextTimeProperty
          pure $
            FilterSnd $
              FilterWithinCursor $
                FilterEntryProperties $
                  FilterMapVal pn $
                    FilterMaybe False $
                      FilterPropertyTime $
                        FilterMaybe False $
                          FilterOrd
                            LEC
                            t
      currentFilter :: Maybe EntryFilter
      currentFilter = filterMWithBase $ combineMFilter totalCurrent workReportContextAdditionalFilter
      matchesSelectedContext = maybe True (`filterPredicate` (rp, fc)) currentFilter
      matchesAnyContext =
        any (\f -> filterPredicate (filterWithBase f) (rp, fc)) $ M.elems workReportContextContexts
      matchesNoContext = not matchesAnyContext
      allAgendaQuadruples :: [(Path Rel File, ForestCursor Entry, TimestampName, Timestamp)]
      allAgendaQuadruples = makeAgendaQuadruples rp fc
      agendaQuadruples :: [(Path Rel File, ForestCursor Entry, TimestampName, Timestamp)]
      agendaQuadruples =
        let go (_, _, tsn, ts) =
              let day = timestampDay ts
                  today = localDay (zonedTimeToLocalTime workReportContextNow)
               in case tsn of
                    "SCHEDULED" -> day <= today
                    "DEADLINE" -> day <= addDays 7 today
                    "BEGIN" -> False
                    "END" -> False
                    _ -> day == today
         in filter go allAgendaQuadruples
      beginEntries :: [(Path Rel File, ForestCursor Entry, TimestampName, Timestamp)]
      beginEntries =
        let go (_, _, tsn, ts) = case tsn of
              "BEGIN" -> timestampLocalTime ts >= zonedTimeToLocalTime workReportContextNow
              _ -> False
         in sortAgendaQuadruples $ filter go allAgendaQuadruples
      nextBeginEntry :: Maybe (Path Rel File, ForestCursor Entry, TimestampName, Timestamp)
      nextBeginEntry = headMay beginEntries
      mWaitingEntry :: Maybe (Path Rel File, ForestCursor Entry, UTCTime, Maybe Time)
      mWaitingEntry = do
        tup@(_, _, ts, mThreshold) <- makeWaitingQuadruple rp fc
        let diff = diffUTCTime (zonedTimeToUTC workReportContextNow) ts
        let threshold = fromMaybe workReportContextWaitingThreshold mThreshold
        guard (diff >= timeNominalDiffTime threshold)
        pure tup
   in IntermediateWorkReport
        { intermediateWorkReportResultEntries = match matchesSelectedContext,
          intermediateWorkReportAgendaEntries = agendaQuadruples,
          intermediateWorkReportNextBegin = nextBeginEntry,
          intermediateWorkReportOverdueWaiting = maybeToList mWaitingEntry,
          intermediateWorkReportOverdueStuck = [],
          intermediateWorkReportEntriesWithoutContext =
            match $
              maybe True (\f -> filterPredicate f (rp, fc)) workReportContextBaseFilter
                && matchesNoContext,
          intermediateWorkReportCheckViolations =
            if matchesAnyContext || matchesSelectedContext
              then
                let go :: EntryFilter -> Maybe (Path Rel File, ForestCursor Entry)
                    go f =
                      if filterPredicate (filterWithBase f) (rp, fc)
                        then Nothing
                        else Just (rp, fc)
                 in M.map (: []) . M.mapMaybe id $ M.fromSet go workReportContextChecks
              else M.empty
        }

data WorkReport = WorkReport
  { workReportResultEntries :: ![(Path Rel File, ForestCursor Entry)],
    workReportAgendaEntries :: ![AgendaEntry],
    workReportNextBegin :: !(Maybe AgendaEntry),
    workReportOverdueWaiting :: ![WaitingEntry],
    workReportOverdueStuck :: ![StuckReportEntry],
    workReportEntriesWithoutContext :: ![(Path Rel File, ForestCursor Entry)],
    workReportCheckViolations :: !(Map EntryFilter [(Path Rel File, ForestCursor Entry)])
  }
  deriving (Show, Eq, Generic)

instance Validity WorkReport where
  validate wr@WorkReport {..} =
    mconcat
      [ genericValidate wr,
        declare "The agenda entries are sorted" $ sortAgendaEntries workReportAgendaEntries == workReportAgendaEntries
      ]

finishWorkReport :: ZonedTime -> Maybe PropertyName -> Maybe Time -> Maybe Sorter -> IntermediateWorkReport -> WorkReport
finishWorkReport now mpn mt ms wr =
  let sortCursorList = maybe id sorterSortCursorList ms
      mAutoFilter :: Maybe EntryFilter
      mAutoFilter = createAutoFilter now mpn (fth <$> intermediateWorkReportNextBegin wr)
      applyAutoFilter :: [(Path Rel File, ForestCursor Entry)] -> [(Path Rel File, ForestCursor Entry)]
      applyAutoFilter = filter $ \tup -> case mAutoFilter of
        Nothing -> True
        Just autoFilter -> case mt of
          Nothing -> filterPredicate autoFilter tup
          Just _ -> True
   in WorkReport
        { workReportAgendaEntries = sortAgendaEntries $ map agendaQuadrupleToAgendaEntry $ intermediateWorkReportAgendaEntries wr,
          workReportResultEntries = sortCursorList $ applyAutoFilter $ intermediateWorkReportResultEntries wr,
          workReportNextBegin = agendaQuadrupleToAgendaEntry <$> intermediateWorkReportNextBegin wr,
          workReportOverdueWaiting = sortWaitingEntries $ map waitingQuadrupleToWaitingEntry $ intermediateWorkReportOverdueWaiting wr,
          workReportOverdueStuck = sortStuckEntries $ intermediateWorkReportOverdueStuck wr,
          workReportEntriesWithoutContext = sortCursorList $ intermediateWorkReportEntriesWithoutContext wr,
          workReportCheckViolations = intermediateWorkReportCheckViolations wr
        }

createAutoFilter :: ZonedTime -> Maybe PropertyName -> Maybe Timestamp -> Maybe EntryFilter
createAutoFilter now mpn mNextBegin = do
  ats <- mNextBegin
  let t = Seconds $ round $ diffUTCTime (localTimeToUTC (zonedTimeZone now) $ timestampLocalTime ats) (zonedTimeToUTC now)
  pn <- mpn
  pure $
    FilterSnd $
      FilterWithinCursor $
        FilterEntryProperties $
          FilterMapVal pn $
            FilterMaybe False $
              FilterPropertyTime $
                FilterMaybe False $
                  FilterOrd LEC t

fth :: (a, b, c, d) -> d
fth (_, _, _, d) = d
