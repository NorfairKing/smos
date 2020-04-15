{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Agenda
  ( smosQueryAgenda,
  )
where

import Conduit
import qualified Data.Conduit.Combinators as C
import Data.List
import Data.Text (Text)
import Data.Time
import Rainbow
import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types
import Smos.Query.Streaming
import Smos.Report.Agenda
import Smos.Report.Streaming
import Smos.Report.TimeBlock

smosQueryAgenda :: AgendaSettings -> Q ()
smosQueryAgenda AgendaSettings {..} = do
  now <- liftIO getZonedTime
  tups <-
    sourceToList $
      streamSmosFiles agendaSetHideArchive .| parseSmosFiles .| printShouldPrint PrintWarning
        .| smosFileCursors
        .| smosMFilter agendaSetFilter
        .| smosCursorCurrents
        .| C.concatMap (uncurry makeAgendaEntry)
        .| C.filter (fitsHistoricity now agendaSetHistoricity)
  liftIO $ putTableLn $ renderAgendaReport now $ makeAgendaReport now agendaSetBlock tups

renderAgendaReport :: ZonedTime -> AgendaReport -> Table
renderAgendaReport now AgendaReport {..} =
  formatAsTable
    $ intercalate [[chunk ""]]
    $ filter
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
