{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Work where

import Conduit
import Cursor.Simple.Forest
import qualified Data.Conduit.Combinators as C
import Data.Function
import qualified Data.Map as M
import Data.Map (Map)
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
import Smos.Report.Sorter
import Smos.Report.Streaming
import Smos.Report.Time

data IntermediateWorkReport
  = IntermediateWorkReport
      { intermediateWorkReportResultEntries :: [(Path Rel File, ForestCursor Entry)],
        intermediateWorkReportEntriesWithoutContext :: [(Path Rel File, ForestCursor Entry)],
        intermediateWorkReportAgendaEntries :: [AgendaEntry],
        intermediateWorkReportNextBegin :: Maybe AgendaEntry,
        intermediateWorkReportCheckViolations :: Map EntryFilterRel [(Path Rel File, ForestCursor Entry)]
      }
  deriving (Show, Eq, Generic)

instance Validity IntermediateWorkReport

instance Semigroup IntermediateWorkReport where
  wr1 <> wr2 =
    IntermediateWorkReport
      { intermediateWorkReportResultEntries = intermediateWorkReportResultEntries wr1 <> intermediateWorkReportResultEntries wr2,
        intermediateWorkReportEntriesWithoutContext =
          intermediateWorkReportEntriesWithoutContext wr1 <> intermediateWorkReportEntriesWithoutContext wr2,
        intermediateWorkReportAgendaEntries = intermediateWorkReportAgendaEntries wr1 <> intermediateWorkReportAgendaEntries wr2,
        intermediateWorkReportCheckViolations =
          M.unionWith (++) (intermediateWorkReportCheckViolations wr1) (intermediateWorkReportCheckViolations wr2),
        intermediateWorkReportNextBegin = case (intermediateWorkReportNextBegin wr1, intermediateWorkReportNextBegin wr2) of
          (Nothing, Nothing) -> Nothing
          (Just ae, Nothing) -> Just ae
          (Nothing, Just ae) -> Just ae
          (Just ae1, Just ae2) ->
            Just $
              if ((<=) `on` (timestampLocalTime . agendaEntryTimestamp)) ae1 ae2
                then ae1
                else ae2
      }

instance Monoid IntermediateWorkReport where
  mempty =
    IntermediateWorkReport
      { intermediateWorkReportResultEntries = mempty,
        intermediateWorkReportEntriesWithoutContext = mempty,
        intermediateWorkReportAgendaEntries = [],
        intermediateWorkReportNextBegin = Nothing,
        intermediateWorkReportCheckViolations = M.empty
      }

data WorkReportContext
  = WorkReportContext
      { workReportContextNow :: ZonedTime,
        workReportContextBaseFilter :: Maybe EntryFilterRel,
        workReportContextCurrentContext :: EntryFilterRel,
        workReportContextTimeProperty :: PropertyName,
        workReportContextTime :: Maybe Time,
        workReportContextAdditionalFilter :: Maybe EntryFilterRel,
        workReportContextContexts :: Map ContextName EntryFilterRel,
        workReportContextChecks :: Set EntryFilterRel,
        workReportContextSorter :: Maybe Sorter
      }
  deriving (Show, Generic)

instance Validity WorkReportContext

produceWorkReport :: MonadIO m => HideArchive -> DirectoryConfig -> WorkReportContext -> m WorkReport
produceWorkReport ha dc wrc = produceReport ha dc $ workReportConduit (workReportContextNow wrc) wrc

workReportConduit :: Monad m => ZonedTime -> WorkReportContext -> ConduitT (Path Rel File, SmosFile) void m WorkReport
workReportConduit now wrc@WorkReportContext {..} =
  fmap (finishWorkReport now workReportContextTimeProperty workReportContextSorter) $
    smosFileCursors .| C.map (uncurry $ makeIntermediateWorkReport wrc) .| accumulateMonoid

makeIntermediateWorkReport :: WorkReportContext -> Path Rel File -> ForestCursor Entry -> IntermediateWorkReport
makeIntermediateWorkReport WorkReportContext {..} rp fc =
  let match b = [(rp, fc) | b]
      combineFilter f = maybe f (FilterAnd f)
      filterWithBase f = combineFilter f workReportContextBaseFilter
      totalCurrent =
        combineFilter workReportContextCurrentContext $
          FilterSnd
            . FilterWithinCursor
            . FilterEntryProperties
            . FilterMapVal workReportContextTimeProperty
            . FilterMaybe False
            . FilterPropertyTime
            . FilterMaybe False
            . FilterOrd LEC <$> workReportContextTime
      currentFilter = filterWithBase $ combineFilter totalCurrent workReportContextAdditionalFilter
      matchesSelectedContext = filterPredicate currentFilter (rp, fc)
      matchesAnyContext =
        any (\f -> filterPredicate (filterWithBase f) (rp, fc)) $ M.elems workReportContextContexts
      matchesNoContext = not matchesAnyContext
      allAgendaEntries = makeAgendaEntry rp $ forestCursorCurrent fc
      agendaEntries =
        let go ae =
              let day = timestampDay (agendaEntryTimestamp ae)
                  today = localDay (zonedTimeToLocalTime workReportContextNow)
               in case agendaEntryTimestampName ae of
                    "SCHEDULED" -> day <= today
                    "DEADLINE" -> day <= addDays 7 today
                    "BEGIN" -> False
                    "END" -> False
                    _ -> day == today
         in filter go allAgendaEntries
      beginEntries =
        let go ae = case agendaEntryTimestampName ae of
              "BEGIN" -> timestampLocalTime (agendaEntryTimestamp ae) >= zonedTimeToLocalTime workReportContextNow
              _ -> False
         in sortAgendaEntries $ filter go allAgendaEntries
   in IntermediateWorkReport
        { intermediateWorkReportResultEntries = match matchesSelectedContext,
          intermediateWorkReportEntriesWithoutContext =
            match $
              maybe True (\f -> filterPredicate f (rp, fc)) workReportContextBaseFilter
                && matchesNoContext,
          intermediateWorkReportAgendaEntries = agendaEntries,
          intermediateWorkReportNextBegin = headMay beginEntries,
          intermediateWorkReportCheckViolations =
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

data WorkReport
  = WorkReport
      { workReportResultEntries :: [(Path Rel File, ForestCursor Entry)],
        workReportEntriesWithoutContext :: [(Path Rel File, ForestCursor Entry)],
        workReportAgendaEntries :: [AgendaEntry],
        workReportNextBegin :: Maybe AgendaEntry,
        workReportCheckViolations :: Map EntryFilterRel [(Path Rel File, ForestCursor Entry)]
      }
  deriving (Show, Eq, Generic)

instance Validity WorkReport where
  validate wr@WorkReport {..} =
    mconcat
      [ genericValidate wr,
        declare "The agenda entries are sorted" $ sortAgendaEntries workReportAgendaEntries == workReportAgendaEntries
      ]

finishWorkReport :: ZonedTime -> PropertyName -> Maybe Sorter -> IntermediateWorkReport -> WorkReport
finishWorkReport now pn ms wr =
  let sortCursorList = maybe id sorterSortCursorList ms
      mAutoFilter :: Maybe EntryFilterRel
      mAutoFilter = do
        ae <- intermediateWorkReportNextBegin wr
        let t = Seconds $ round $ diffUTCTime (localTimeToUTC (zonedTimeZone now) $ timestampLocalTime $ agendaEntryTimestamp ae) (zonedTimeToUTC now)
        pure $ FilterSnd
          $ FilterWithinCursor
          $ FilterEntryProperties
          $ FilterMapVal pn
          $ FilterMaybe False
          $ FilterPropertyTime
          $ FilterMaybe False
          $ FilterOrd LEC t
      applyAutoFilter = filter $ \tup -> case mAutoFilter of
        Nothing -> True
        Just autoFilter -> filterPredicate autoFilter tup
   in WorkReport
        { workReportAgendaEntries = sortAgendaEntries $ intermediateWorkReportAgendaEntries wr,
          workReportResultEntries = sortCursorList $ applyAutoFilter $ intermediateWorkReportResultEntries wr,
          workReportNextBegin = intermediateWorkReportNextBegin wr,
          workReportEntriesWithoutContext = sortCursorList $ intermediateWorkReportEntriesWithoutContext wr,
          workReportCheckViolations = intermediateWorkReportCheckViolations wr
        }
