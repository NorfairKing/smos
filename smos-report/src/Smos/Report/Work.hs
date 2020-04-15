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
import Smos.Data
import Smos.Report.Agenda
import Smos.Report.Config
import Smos.Report.Filter
import Smos.Report.Path
import Smos.Report.Sorter

data WorkReport
  = WorkReport
      { workReportResultEntries :: [(RootedPath, ForestCursor Entry)],
        workReportEntriesWithoutContext :: [(RootedPath, ForestCursor Entry)],
        workReportAgendaEntries :: [AgendaEntry],
        workReportCheckViolations :: Map EntryFilter [(RootedPath, ForestCursor Entry)]
      }
  deriving (Show, Eq, Generic)

instance Validity WorkReport

instance Semigroup WorkReport where
  wr1 <> wr2 =
    WorkReport
      { workReportResultEntries = workReportResultEntries wr1 <> workReportResultEntries wr2,
        workReportEntriesWithoutContext =
          workReportEntriesWithoutContext wr1 <> workReportEntriesWithoutContext wr2,
        workReportAgendaEntries = workReportAgendaEntries wr1 <> workReportAgendaEntries wr2,
        workReportCheckViolations =
          M.unionWith (++) (workReportCheckViolations wr1) (workReportCheckViolations wr2)
      }

instance Monoid WorkReport where
  mempty =
    WorkReport
      { workReportResultEntries = mempty,
        workReportEntriesWithoutContext = mempty,
        workReportAgendaEntries = [],
        workReportCheckViolations = M.empty
      }

data WorkReportContext
  = WorkReportContext
      { workReportContextNow :: ZonedTime,
        workReportContextBaseFilter :: Maybe EntryFilter,
        workReportContextCurrentContext :: EntryFilter,
        workReportContextTimeFilter :: Maybe (Filter Entry),
        workReportContextAdditionalFilter :: Maybe EntryFilter,
        workReportContextContexts :: Map ContextName EntryFilter,
        workReportContextChecks :: Set EntryFilter
      }
  deriving (Show, Generic)

makeWorkReport :: WorkReportContext -> RootedPath -> ForestCursor Entry -> WorkReport
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
   in WorkReport
        { workReportResultEntries = match matchesSelectedContext,
          workReportEntriesWithoutContext =
            match $
              maybe True (\f -> filterPredicate f (rp, fc)) workReportContextBaseFilter
                && matchesNoContext,
          workReportAgendaEntries =
            let go ae =
                  let day = timestampDay (agendaEntryTimestamp ae)
                      today = localDay (zonedTimeToLocalTime workReportContextNow)
                   in case agendaEntryTimestampName ae of
                        "SCHEDULED" -> day <= today
                        "DEADLINE" -> day <= addDays 7 today
                        _ -> day == today
             in filter go $ makeAgendaEntry rp $ forestCursorCurrent fc,
          workReportCheckViolations =
            if matchesAnyContext
              then
                let go :: EntryFilter -> Maybe (RootedPath, ForestCursor Entry)
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
        { workReportAgendaEntries = workReportAgendaEntries wr,
          workReportResultEntries = sorterSortCursorList s $ workReportResultEntries wr,
          workReportEntriesWithoutContext =
            sorterSortCursorList s $ workReportEntriesWithoutContext wr,
          workReportCheckViolations = workReportCheckViolations wr
        }
