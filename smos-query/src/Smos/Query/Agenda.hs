{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.Agenda where

import Data.List
import Data.Ord
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time
import Text.Time.Pretty

import Conduit
import qualified Data.Conduit.Combinators as C
import Rainbow

import Smos.Data

import Smos.Report.Agenda
import Smos.Report.Filter
import Smos.Report.Streaming
import Smos.Report.TimeBlock

import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types
import Smos.Query.Streaming

agenda :: AgendaSettings -> Q ()
agenda AgendaSettings {..} = do
  now <- liftIO getZonedTime
  tups <-
    sourceToList $
    streamSmosFiles agendaSetHideArchive .| parseSmosFiles .| printShouldPrint PrintWarning .|
    smosFileCursors .|
    C.filter (\(rp, fc) -> maybe True (\f -> filterPredicate f rp fc) agendaSetFilter) .|
    smosCursorCurrents .|
    C.concatMap (uncurry makeAgendaEntry) .|
    C.filter (fitsHistoricity now agendaSetHistoricity)
  liftIO $ putTableLn $ renderAgendaReport now $ divideIntoAgendaTableBlocks agendaSetBlock tups

renderAgendaReport :: ZonedTime -> [AgendaTableBlock Text] -> Table
renderAgendaReport now atbs =
  formatAsTable $
  case atbs of
    [] -> []
    [atb] -> goEntries (blockEntries atb)
    _ -> concatMap goEntriesWithTitle atbs
  where
    goEntriesWithTitle Block {..} = [fore blue $ chunk blockTitle] : goEntries blockEntries
    goEntries es = renderSplit . splitUp $ (sortAgendaEntries es)
    splitUp =
      splitList $ \ae ->
        compare (timestampDay $ agendaEntryTimestamp ae) (localDay $ zonedTimeToLocalTime now)
    renderSplit (before, during, after) =
      case (go before, go during, go after) of
        (xs, [], []) -> concat [xs]
        ([], ys, []) -> concat [ys]
        ([], [], zs) -> concat [zs]
        (xs, ys, []) -> concat [xs, [[chunk ""]], ys]
        ([], ys, zs) -> concat [ys, [[chunk ""]], zs]
        (xs, [], zs) -> concat [xs, [[chunk ""]], zs]
        (xs, ys, zs) -> concat [xs, [[chunk ""]], ys, [[chunk ""]], zs]
      where
        go = map (formatAgendaEntry now)

sortAgendaEntries :: [AgendaEntry] -> [AgendaEntry]
sortAgendaEntries =
  sortBy
    (mconcat
       [ comparing (timestampLocalTime . agendaEntryTimestamp)
       , comparing agendaEntryTimestampName
       , comparing agendaEntryTodoState
       ])

splitList :: (a -> Ordering) -> [a] -> ([a], [a], [a])
splitList func = go
  where
    go [] = ([], [], [])
    go (a:as) =
      case func a of
        LT ->
          case go as of
            (xs, ys, zs) -> (a : xs, ys, zs)
        EQ ->
          case go2 as of
            (ys, zs) -> ([], a : ys, zs)
        GT -> ([], [], a : as)
    go2 [] = ([], [])
    go2 (a:as) =
      case func a of
        LT -> error "should not happen"
        EQ ->
          case go2 as of
            (ys, zs) -> (a : ys, zs)
        GT -> ([], a : as)

formatAgendaEntry :: ZonedTime -> AgendaEntry -> [Chunk Text]
formatAgendaEntry now AgendaEntry {..} =
  let d = diffDays (timestampDay agendaEntryTimestamp) (localDay $ zonedTimeToLocalTime now)
      func =
        if | d <= 0 && agendaEntryTimestampName == "DEADLINE" -> fore red
           | d == 1 && agendaEntryTimestampName == "DEADLINE" -> fore brightRed . back black
           | d <= 10 && agendaEntryTimestampName == "DEADLINE" -> fore yellow
           | d < 0 && agendaEntryTimestampName == "SCHEDULED" -> fore red
           | d == 0 && agendaEntryTimestampName == "SCHEDULED" -> fore green
           | otherwise -> id
   in [ func $ rootedPathChunk agendaEntryFilePath
      , func $ chunk $ timestampPrettyText agendaEntryTimestamp
      , func $ chunk $ T.pack $ renderDaysAgoAuto $ daysAgo $ negate d
      , timestampNameChunk $ agendaEntryTimestampName
      , maybe (chunk "") todoStateChunk agendaEntryTodoState
      , headerChunk agendaEntryHeader
      ]
