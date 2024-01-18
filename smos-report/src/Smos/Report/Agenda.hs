{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Agenda where

import Conduit
import Control.DeepSeq
import Cursor.Forest
import Data.Aeson
import qualified Data.Conduit.Combinators as C
import Data.Function
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Data.Text (Text)
import Data.Time
import Data.Validity
import GHC.Generics (Generic)
import Path
import Smos.Data
import Smos.Directory.Archive
import Smos.Directory.OptParse.Types
import Smos.Directory.Resolution
import Smos.Directory.ShouldPrint
import Smos.Directory.Streaming
import Smos.Report.Agenda.Types
import Smos.Report.Filter
import Smos.Report.Period
import Smos.Report.TimeBlock

produceAgendaReport ::
  (MonadIO m) =>
  Day ->
  Period ->
  TimeBlock ->
  HideArchive ->
  ShouldPrint ->
  AgendaHistoricity ->
  Maybe EntryFilter ->
  DirectorySettings ->
  m AgendaReport
produceAgendaReport today period timeBlock ha sp h f dc = do
  wd <- liftIO $ resolveDirWorkflowDir dc
  runConduit $
    streamSmosFilesFromWorkflowRel ha dc
      .| produceAgendaReportFromFiles today period timeBlock h f sp wd

produceAgendaReportFromFiles ::
  (MonadIO m) =>
  Day ->
  Period ->
  TimeBlock ->
  AgendaHistoricity ->
  Maybe EntryFilter ->
  ShouldPrint ->
  Path Abs Dir ->
  ConduitT (Path Rel File) void m AgendaReport
produceAgendaReportFromFiles today p tb h f sp wd = do
  filterSmosFilesRel
    .| parseSmosFilesRel wd
    .| printShouldPrint sp
    .| agendaReportConduit today p tb h f

agendaReportConduit ::
  (Monad m) =>
  Day ->
  Period ->
  TimeBlock ->
  AgendaHistoricity ->
  Maybe EntryFilter ->
  ConduitT (Path Rel File, SmosFile) void m AgendaReport
agendaReportConduit today p tb h mf =
  makeAgendaReport today p tb
    <$> ( smosFileCursors
            .| mFilterConduit mf
            .| C.concatMap (uncurry makeAgendaEntry)
            .| C.filter (fitsHistoricity today h)
            .| sinkList
        )

data AgendaReport = AgendaReport
  { agendaReportPast :: [AgendaTableBlock Text],
    agendaReportPresent :: AgendaTodayReport,
    agendaReportFuture :: [AgendaTableBlock Text]
  }
  deriving (Show, Eq, Generic)

instance Validity AgendaReport

instance NFData AgendaReport

instance FromJSON AgendaReport where
  parseJSON = withObject "AgendaReport" $ \o -> AgendaReport <$> o .: "past" <*> o .: "present" <*> o .: "future"

instance ToJSON AgendaReport where
  toJSON AgendaReport {..} = object ["past" .= agendaReportPast, "present" .= agendaReportPresent, "future" .= agendaReportFuture]

makeAgendaReport ::
  Day ->
  Period ->
  TimeBlock ->
  [AgendaEntry] ->
  AgendaReport
makeAgendaReport today period tb as =
  let interval = periodInterval today period
      filteredAgenda = filter (filterIntervalTimestamp interval . agendaEntryTimestamp) as
      (past, present, future) = divideIntoPastPresentFuture today filteredAgenda
      pastBlocks = divideIntoAgendaTableBlocks tb past
      futureBlocks = divideIntoAgendaTableBlocks tb future
   in AgendaReport
        { agendaReportPast = pastBlocks,
          agendaReportPresent =
            AgendaTodayReport
              { agendaTodayReportEntries = present
              },
          agendaReportFuture = futureBlocks
        }

divideIntoPastPresentFuture ::
  Day ->
  [AgendaEntry] ->
  ([AgendaEntry], [AgendaEntry], [AgendaEntry])
divideIntoPastPresentFuture today =
  splitList
    ( \ae ->
        compare (timestampDay $ agendaEntryTimestamp ae) today
    )
    . sortAgendaEntries
  where
    splitList :: (a -> Ordering) -> [a] -> ([a], [a], [a])
    splitList func = go
      where
        go [] = ([], [], [])
        go (a : as) =
          case func a of
            LT ->
              case go as of
                (xs, ys, zs) -> (a : xs, ys, zs)
            EQ ->
              case go2 as of
                (ys, zs) -> ([], a : ys, zs)
            GT -> ([], [], a : as)
        go2 [] = ([], [])
        go2 (a : as) =
          case func a of
            LT -> error "should not happen"
            EQ ->
              case go2 as of
                (ys, zs) -> (a : ys, zs)
            GT -> ([], a : as)

newtype AgendaTodayReport = AgendaTodayReport
  { agendaTodayReportEntries :: [AgendaEntry]
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance Validity AgendaTodayReport where
  validate atr@AgendaTodayReport {..} =
    mconcat
      [ genericValidate atr,
        declare "The entries are in chronological order" $
          agendaEntriesAreSorted agendaTodayReportEntries,
        declare "All the entries are in the same day" $
          (<= 1) $
            length $
              group $
                map (timestampDay . agendaEntryTimestamp) agendaTodayReportEntries
      ]

instance NFData AgendaTodayReport

type AgendaTableBlock a = Block a AgendaEntry

divideIntoAgendaTableBlocks :: TimeBlock -> [AgendaEntry] -> [AgendaTableBlock Text]
divideIntoAgendaTableBlocks = divideIntoBlocks (timestampDay . agendaEntryTimestamp)

agendaEntriesAreSorted :: [AgendaEntry] -> Bool
agendaEntriesAreSorted = areOrderedBy agendaEntrySortingProjection

areOrderedBy :: (Eq a) => (a -> a -> Ordering) -> [a] -> Bool
areOrderedBy func ls =
  sortBy func ls == ls

sortAgendaEntries :: [AgendaEntry] -> [AgendaEntry]
sortAgendaEntries =
  sortBy agendaEntrySortingProjection

agendaEntrySortingProjection :: AgendaEntry -> AgendaEntry -> Ordering
agendaEntrySortingProjection =
  mconcat
    [ comparing (timestampLocalTime . agendaEntryTimestamp),
      comparing (Down . agendaEntryTimestampName),
      comparing agendaEntryTodoState
    ]

sortAgendaQuadruples ::
  [(Path Rel File, ForestCursor Entry Entry, TimestampName, Timestamp)] ->
  [(Path Rel File, ForestCursor Entry Entry, TimestampName, Timestamp)]
sortAgendaQuadruples = sortBy agendaQuadrupleSortingProjection

agendaQuadrupleSortingProjection ::
  (Path Rel File, ForestCursor Entry Entry, TimestampName, Timestamp) ->
  (Path Rel File, ForestCursor Entry Entry, TimestampName, Timestamp) ->
  Ordering
agendaQuadrupleSortingProjection = agendaEntrySortingProjection `on` agendaQuadrupleToAgendaEntry

data AgendaEntry = AgendaEntry
  { agendaEntryFilePath :: Path Rel File,
    agendaEntryHeader :: Header,
    agendaEntryTodoState :: Maybe TodoState,
    agendaEntryTimestampName :: TimestampName,
    agendaEntryTimestamp :: Timestamp
  }
  deriving (Show, Eq, Generic)

instance Validity AgendaEntry

instance NFData AgendaEntry

instance FromJSON AgendaEntry

instance ToJSON AgendaEntry

makeAgendaEntry :: Path Rel File -> ForestCursor Entry Entry -> [AgendaEntry]
makeAgendaEntry rf fc = agendaQuadrupleToAgendaEntry <$> makeAgendaQuadruples rf fc

agendaQuadrupleToAgendaEntry :: (Path Rel File, ForestCursor Entry Entry, TimestampName, Timestamp) -> AgendaEntry
agendaQuadrupleToAgendaEntry (rf, fc, tsn, ts) =
  let e = forestCursorCurrent fc
   in AgendaEntry
        { agendaEntryFilePath = rf,
          agendaEntryHeader = entryHeader e,
          agendaEntryTodoState = entryState e,
          agendaEntryTimestampName = tsn,
          agendaEntryTimestamp = ts
        }

makeAgendaQuadruples :: Path Rel File -> ForestCursor Entry Entry -> [(Path Rel File, ForestCursor Entry Entry, TimestampName, Timestamp)]
makeAgendaQuadruples rf fc =
  let e = forestCursorCurrent fc
   in flip mapMaybe (M.toList $ entryTimestamps e) $ \(tsn, ts) ->
        if mTodoStateIsDone (entryState e)
          then Nothing
          else Just (rf, fc, tsn, ts)

fitsHistoricity :: Day -> AgendaHistoricity -> AgendaEntry -> Bool
fitsHistoricity today ah ae =
  case ah of
    HistoricalAgenda -> True
    FutureAgenda -> timestampDay (agendaEntryTimestamp ae) >= today
