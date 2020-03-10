{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.Commands.Agenda where

import Data.List
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time
import Text.Time.Pretty

import Conduit
import qualified Data.Conduit.Combinators as C
import Rainbow

import Smos.Data

import Smos.Report.Agenda
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
    smosMFilter agendaSetFilter .|
    smosCursorCurrents .|
    C.concatMap (uncurry makeAgendaEntry) .|
    C.filter (fitsHistoricity now agendaSetHistoricity)
  liftIO $ putTableLn $ renderAgendaReport now $ makeAgendaReport now agendaSetBlock tups

renderAgendaReport :: ZonedTime -> AgendaReport -> Table
renderAgendaReport now AgendaReport {..} =
  formatAsTable $
  intercalate [[chunk ""]] $
  filter
    (not . null)
    [goBlocks agendaReportPast, goBlocks agendaReportPresent, goBlocks agendaReportFuture]
  where
    goBlocks bs =
      case bs of
        [] -> []
        [b] -> goEntries $ blockEntries b
        _ -> concatMap goBlock bs
    goBlock Block {..} = [fore blue $ chunk blockTitle] : goEntries blockEntries
    goEntries :: [AgendaEntry] -> [[Chunk Text]]
    goEntries = map (formatAgendaEntry now)

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
      , timestampNameChunk agendaEntryTimestampName
      , maybe (chunk "") todoStateChunk agendaEntryTodoState
      , headerChunk agendaEntryHeader
      ]
