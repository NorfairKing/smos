{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Smos.Report.Work where

import Conduit
import Control.Monad
import Cursor.Simple.Forest
import qualified Data.Conduit.Combinators as C
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import Data.Time
import Data.Time.Zones
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics (Generic)
import Path
import Safe
import Smos.Data
import Smos.Directory.Archive
import Smos.Directory.OptParse.Types
import Smos.Directory.ShouldPrint
import Smos.Directory.Streaming
import Smos.Report.Agenda
import Smos.Report.Comparison
import Smos.Report.Filter
import Smos.Report.OptParse.Types
import Smos.Report.Projects
import Smos.Report.Sorter
import Smos.Report.Stuck
import Smos.Report.Time
import Smos.Report.Waiting

produceWorkReport :: MonadIO m => HideArchive -> ShouldPrint -> DirectorySettings -> WorkReportContext -> m WorkReport
produceWorkReport ha sp dc wrc = produceReport ha sp dc $ workReportConduit wrc

workReportConduit :: Monad m => WorkReportContext -> ConduitT (Path Rel File, SmosFile) void m WorkReport
workReportConduit wrc@WorkReportContext {..} =
  finishWorkReport
    workReportContextTimeZone
    workReportContextNow
    workReportContextTimeProperty
    workReportContextTime
    workReportContextSorter
    <$> intermediateWorkReportConduit wrc

intermediateWorkReportConduit :: Monad m => WorkReportContext -> ConduitT (Path Rel File, SmosFile) void m IntermediateWorkReport
intermediateWorkReportConduit wrc =
  C.map (uncurry $ makeIntermediateWorkReportForFile wrc) .| C.fold

data IntermediateWorkReport = IntermediateWorkReport
  { intermediateWorkReportResultEntries :: !(DList (Path Rel File, ForestCursor Entry)),
    intermediateWorkReportAgendaEntries :: !(DList (Path Rel File, ForestCursor Entry, TimestampName, Timestamp)),
    intermediateWorkReportNextBegin :: !(Maybe (Path Rel File, ForestCursor Entry, TimestampName, Timestamp)),
    intermediateWorkReportOverdueWaiting :: !(DList (Path Rel File, ForestCursor Entry, UTCTime, Maybe Time)),
    intermediateWorkReportOverdueStuck :: !(DList StuckReportEntry),
    intermediateWorkReportLimboProjects :: !(DList (Path Rel File)),
    intermediateWorkReportEntriesWithoutContext :: !(DList (Path Rel File, ForestCursor Entry)),
    intermediateWorkReportCheckViolations :: !(Map EntryFilter (DList (Path Rel File, ForestCursor Entry)))
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
        intermediateWorkReportLimboProjects = intermediateWorkReportLimboProjects wr1 <> intermediateWorkReportLimboProjects wr2,
        intermediateWorkReportCheckViolations =
          M.unionWith (<>) (intermediateWorkReportCheckViolations wr1) (intermediateWorkReportCheckViolations wr2),
        intermediateWorkReportEntriesWithoutContext =
          intermediateWorkReportEntriesWithoutContext wr1 <> intermediateWorkReportEntriesWithoutContext wr2
      }

instance Monoid IntermediateWorkReport where
  mempty =
    IntermediateWorkReport
      { intermediateWorkReportResultEntries = mempty,
        intermediateWorkReportAgendaEntries = mempty,
        intermediateWorkReportNextBegin = Nothing,
        intermediateWorkReportOverdueWaiting = mempty,
        intermediateWorkReportOverdueStuck = mempty,
        intermediateWorkReportLimboProjects = mempty,
        intermediateWorkReportEntriesWithoutContext = mempty,
        intermediateWorkReportCheckViolations = M.empty
      }

data WorkReportContext = WorkReportContext
  { -- | Current time, for computing whether something is overdue
    workReportContextNow :: !UTCTime,
    -- | Timezone, for interpreting the local times in timestamps
    workReportContextTimeZone :: !TZ,
    -- | Projects, for deciding whether a file is a project
    workReportContextProjectsSubdir :: !(Maybe (Path Rel Dir)),
    -- | Base filter, for filtering out most things and selecting next action entries
    workReportContextBaseFilter :: !(Maybe EntryFilter),
    -- | Filter for the current context
    workReportContextCurrentContext :: !(Maybe EntryFilter),
    -- | The property to filter time by, Nothing means no time filtering
    workReportContextTimeProperty :: !(Maybe PropertyName),
    -- | The time to filter by, Nothing means don't discriminate on time
    workReportContextTime :: !(Maybe Time),
    -- | Additional filter, for an extra filter argument
    workReportContextAdditionalFilter :: !(Maybe EntryFilter),
    -- | Map of contexts, for checking whether any entry has no context
    workReportContextContexts :: !(Map ContextName EntryFilter),
    -- | Extra checks to perform
    workReportContextChecks :: !(Set EntryFilter),
    -- | How to sort the next action entries, Nothing means no sorting
    workReportContextSorter :: !(Maybe Sorter),
    -- | When to consider waiting entries 'overdue'
    workReportContextWaitingThreshold :: !Time,
    -- | When to consider stuck projects 'overdue'
    workReportContextStuckThreshold :: !Time
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
        se <- makeStuckReportEntry workReportContextTimeZone rp sf
        latestChange <- stuckReportEntryLatestChange se
        let diff = diffUTCTime workReportContextNow latestChange
        guard (diff >= timeNominalDiffTime workReportContextStuckThreshold)
        pure se
      mLimboProject :: Maybe (Path Rel File)
      mLimboProject = do
        -- To make sure that only projects are considered
        _ <- case workReportContextProjectsSubdir of
          Nothing -> Just ()
          Just psd -> () <$ stripProperPrefix psd rp
        -- In limbo if there is no current entry
        guard $ isNothing $ getCurrentEntry sf
        pure rp
   in iwr
        { intermediateWorkReportOverdueStuck = maybeToDList mStuckEntry,
          intermediateWorkReportLimboProjects = maybeToDList mLimboProject
        }

makeIntermediateWorkReport :: WorkReportContext -> Path Rel File -> ForestCursor Entry -> IntermediateWorkReport
makeIntermediateWorkReport WorkReportContext {..} rp fc =
  let nowLocal = utcToLocalTimeTZ workReportContextTimeZone workReportContextNow
      today = localDay nowLocal
      match b = if b then DList.singleton (rp, fc) else DList.empty
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
      nowIsAfterAfter =
        case M.lookup "AFTER" (entryTimestamps (forestCursorCurrent fc)) of
          Nothing -> True
          Just afterTimestamp -> case afterTimestamp of
            TimestampDay d -> today > d
            TimestampLocalTime lt -> nowLocal > lt
      matchesSelectedContext =
        maybe True (`filterPredicate` (rp, fc)) currentFilter
      matchesAnyContext =
        any (\f -> filterPredicate (filterWithBase f) (rp, fc)) $ M.elems workReportContextContexts
      matchesNoContext = not matchesAnyContext
      allAgendaQuadruples :: [(Path Rel File, ForestCursor Entry, TimestampName, Timestamp)]
      allAgendaQuadruples = makeAgendaQuadruples rp fc
      agendaQuadruples :: [(Path Rel File, ForestCursor Entry, TimestampName, Timestamp)]
      agendaQuadruples =
        let go (_, _, tsn, ts) =
              let day = timestampDay ts
               in case tsn of
                    "SCHEDULED" -> day <= today
                    "DEADLINE" -> day <= addDays 7 today && nowIsAfterAfter
                    "BEGIN" -> False
                    "END" -> False
                    "AFTER" -> False
                    _ -> day == today
         in filter go allAgendaQuadruples
      beginEntries :: [(Path Rel File, ForestCursor Entry, TimestampName, Timestamp)]
      beginEntries =
        let go (_, _, tsn, ts) = case tsn of
              "BEGIN" -> timestampLocalTime ts >= nowLocal
              _ -> False
         in sortAgendaQuadruples $ filter go allAgendaQuadruples
      nextBeginEntry :: Maybe (Path Rel File, ForestCursor Entry, TimestampName, Timestamp)
      nextBeginEntry = headMay beginEntries
      mWaitingEntry :: Maybe (Path Rel File, ForestCursor Entry, UTCTime, Maybe Time)
      mWaitingEntry = do
        tup@(_, _, ts, mThreshold) <- makeWaitingQuadruple rp fc
        let diff = diffUTCTime workReportContextNow ts
        let threshold = fromMaybe workReportContextWaitingThreshold mThreshold
        guard (diff >= timeNominalDiffTime threshold)
        pure tup
   in IntermediateWorkReport
        { intermediateWorkReportResultEntries = match $ matchesSelectedContext && nowIsAfterAfter,
          intermediateWorkReportAgendaEntries = DList.fromList agendaQuadruples,
          intermediateWorkReportNextBegin = nextBeginEntry,
          intermediateWorkReportOverdueWaiting = maybeToDList mWaitingEntry,
          intermediateWorkReportOverdueStuck = mempty,
          intermediateWorkReportLimboProjects = mempty,
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
                 in M.map DList.singleton . M.mapMaybe id $ M.fromSet go workReportContextChecks
              else M.empty
        }

data WorkReport = WorkReport
  { workReportResultEntries :: ![(Path Rel File, ForestCursor Entry)],
    workReportAgendaEntries :: ![AgendaEntry],
    workReportNextBegin :: !(Maybe AgendaEntry),
    workReportOverdueWaiting :: ![WaitingEntry],
    workReportOverdueStuck :: ![StuckReportEntry],
    workReportLimboProjects :: ![Path Rel File],
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

finishWorkReport :: TZ -> UTCTime -> Maybe PropertyName -> Maybe Time -> Maybe Sorter -> IntermediateWorkReport -> WorkReport
finishWorkReport zone now mpn mt ms wr =
  let sortCursorList = maybe id sorterSortCursorList ms
      mAutoFilter :: Maybe EntryFilter
      mAutoFilter = createAutoFilter zone now mpn (fth <$> intermediateWorkReportNextBegin wr)
      applyAutoFilter :: [(Path Rel File, ForestCursor Entry)] -> [(Path Rel File, ForestCursor Entry)]
      applyAutoFilter = filter $ \tup -> case mAutoFilter of
        Nothing -> True
        Just autoFilter -> case mt of
          Nothing -> filterPredicate autoFilter tup
          Just _ -> True
   in WorkReport
        { workReportAgendaEntries = sortAgendaEntries $ map agendaQuadrupleToAgendaEntry $ DList.toList $ intermediateWorkReportAgendaEntries wr,
          workReportResultEntries = sortCursorList $ applyAutoFilter $ DList.toList $ intermediateWorkReportResultEntries wr,
          workReportNextBegin = agendaQuadrupleToAgendaEntry <$> intermediateWorkReportNextBegin wr,
          workReportOverdueWaiting = sortWaitingEntries $ map waitingQuadrupleToWaitingEntry $ DList.toList $ intermediateWorkReportOverdueWaiting wr,
          workReportOverdueStuck = sortStuckEntries $ DList.toList $ intermediateWorkReportOverdueStuck wr,
          workReportLimboProjects = sort $ DList.toList $ intermediateWorkReportLimboProjects wr,
          workReportEntriesWithoutContext = sortCursorList $ DList.toList $ intermediateWorkReportEntriesWithoutContext wr,
          workReportCheckViolations = M.map DList.toList $ intermediateWorkReportCheckViolations wr
        }

createAutoFilter :: TZ -> UTCTime -> Maybe PropertyName -> Maybe Timestamp -> Maybe EntryFilter
createAutoFilter zone now mpn mNextBegin = do
  ats <- mNextBegin
  let t = Seconds $ round $ diffUTCTime (localTimeToUTCTZ zone (timestampLocalTime ats)) now
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

instance Validity a => Validity (DList a) where
  validate = validate . DList.toList

maybeToDList :: Maybe a -> DList a
maybeToDList = \case
  Nothing -> DList.empty
  Just a -> DList.singleton a
