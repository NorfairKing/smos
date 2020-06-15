{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Work where

import Cursor.Simple.Forest
import qualified Data.Map as M
import Data.Map (Map)
import Data.Set (Set)
import Data.Time
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics (Generic)
import Path
import Smos.Data
import Smos.Report.Agenda
import Smos.Report.Config
import Smos.Report.Filter
import Smos.Report.Sorter

data WorkReport
  = WorkReport
      { workReportResultEntries :: [(Path Rel File, ForestCursor Entry)],
        workReportEntriesWithoutContext :: [(Path Rel File, ForestCursor Entry)],
        workReportAgendaPastEntries :: [AgendaEntry],
        workReportAgendaTodayReport :: AgendaTodayReport,
        workReportAgendaFutureEntries :: [AgendaEntry],
        workReportCheckViolations :: Map EntryFilterRel [(Path Rel File, ForestCursor Entry)]
      }
  deriving (Show, Eq, Generic)

instance Validity WorkReport

instance Semigroup WorkReport where
  wr1 <> wr2 =
    WorkReport
      { workReportResultEntries = workReportResultEntries wr1 <> workReportResultEntries wr2,
        workReportEntriesWithoutContext =
          workReportEntriesWithoutContext wr1 <> workReportEntriesWithoutContext wr2,
        workReportAgendaPastEntries = workReportAgendaPastEntries wr1 <> workReportAgendaPastEntries wr2,
        workReportAgendaTodayReport = workReportAgendaTodayReport wr1 <> workReportAgendaTodayReport wr2,
        workReportAgendaFutureEntries = workReportAgendaFutureEntries wr1 <> workReportAgendaFutureEntries wr2,
        workReportCheckViolations =
          M.unionWith (++) (workReportCheckViolations wr1) (workReportCheckViolations wr2)
      }

instance Monoid WorkReport where
  mempty =
    WorkReport
      { workReportResultEntries = mempty,
        workReportEntriesWithoutContext = mempty,
        workReportAgendaPastEntries = [],
        workReportAgendaTodayReport = mempty,
        workReportAgendaFutureEntries = [],
        workReportCheckViolations = M.empty
      }

data WorkReportContext
  = WorkReportContext
      { workReportContextNow :: ZonedTime,
        workReportContextBaseFilter :: Maybe EntryFilterRel,
        workReportContextCurrentContext :: EntryFilterRel,
        workReportContextTimeFilter :: Maybe (Filter Entry),
        workReportContextAdditionalFilter :: Maybe EntryFilterRel,
        workReportContextContexts :: Map ContextName EntryFilterRel,
        workReportContextChecks :: Set EntryFilterRel
      }
  deriving (Show, Generic)

makeWorkReport :: WorkReportContext -> Path Rel File -> ForestCursor Entry -> WorkReport
makeWorkReport WorkReportContext {..} rp fc =
  let match b = [(rp, fc) | b]
      combineFilter f = maybe f (FilterAnd f)
      filterWithBase f = combineFilter f workReportContextBaseFilter
      totalCurrent =
        combineFilter workReportContextCurrentContext $
          FilterSnd . FilterWithinCursor <$> workReportContextTimeFilter
      currentFilter = filterWithBase $ combineFilter totalCurrent workReportContextAdditionalFilter
      matchesSelectedContext = filterPredicate currentFilter (rp, fc)
      matchesAnyContext =
        any (\f -> filterPredicate (filterWithBase f) (rp, fc)) $ M.elems workReportContextContexts
      matchesNoContext = not matchesAnyContext
      agendaEntries =
        let go ae =
              let day = timestampDay (agendaEntryTimestamp ae)
                  today = localDay (zonedTimeToLocalTime workReportContextNow)
               in case agendaEntryTimestampName ae of
                    "SCHEDULED" -> day <= today
                    "DEADLINE" -> day <= addDays 7 today
                    _ -> day == today
         in filter go $ makeAgendaEntry rp $ forestCursorCurrent fc
      (past, present, future) = divideIntoPastPresentFuture workReportContextNow agendaEntries
   in WorkReport
        { workReportResultEntries = match matchesSelectedContext,
          workReportEntriesWithoutContext =
            match $
              maybe True (\f -> filterPredicate f (rp, fc)) workReportContextBaseFilter
                && matchesNoContext,
          workReportAgendaPastEntries = past,
          workReportAgendaTodayReport = AgendaTodayReport present,
          workReportAgendaFutureEntries = future,
          workReportCheckViolations =
            if matchesAnyContext
              then
                let go :: EntryFilterRel -> Maybe (Path Rel File, ForestCursor Entry)
                    go f =
                      if filterPredicate (filterWithBase f) (rp, fc)
                        then Nothing
                        else Just (rp, fc)
                 in M.map (: []) . M.mapMaybe id $ M.fromSet go workReportContextChecks
              else M.empty
        }

finishWorkReport :: Maybe Sorter -> WorkReport -> WorkReport
finishWorkReport ms wr =
  case ms of
    Nothing -> wr
    Just s ->
      WorkReport
        { workReportAgendaPastEntries = workReportAgendaPastEntries wr,
          workReportAgendaTodayReport = AgendaTodayReport $ sortAgendaEntries $ agendaTodayReportEntries $ workReportAgendaTodayReport wr,
          workReportAgendaFutureEntries = workReportAgendaFutureEntries wr,
          workReportResultEntries = sorterSortCursorList s $ workReportResultEntries wr,
          workReportEntriesWithoutContext =
            sorterSortCursorList s $ workReportEntriesWithoutContext wr,
          workReportCheckViolations = workReportCheckViolations wr
        }
