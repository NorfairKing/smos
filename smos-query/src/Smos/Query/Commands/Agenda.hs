{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Agenda
  ( smosQueryAgenda,
    renderAgendaReportLines,
    addNowLine,
    AgendaReportLine (..),
  )
where

import Conduit
import qualified Data.Conduit.Combinators as C
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import GHC.Generics
import Rainbow
import Smos.Data
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
renderAgendaReport now = formatAsTable . renderAgendaReportLines now . addNowLine now . makeAgendaReportLines

renderAgendaReportLines :: ZonedTime -> [AgendaReportLine] -> [[Chunk Text]]
renderAgendaReportLines now = map $ \case
  TitleLine t -> [fore blue $ chunk t]
  SpaceLine -> [chunk ""]
  NowLine ->
    [ fore yellow $ chunk $ T.pack $
        unwords
          [ "---",
            "Now:",
            formatTime defaultTimeLocale "%A %Y-%m-%d %H:%M:%S" now,
            "---"
          ]
    ]
  EntryLine ae -> formatAgendaEntry now ae

data AgendaReportLine
  = TitleLine Text
  | NowLine
  | SpaceLine
  | EntryLine AgendaEntry
  deriving (Show, Eq, Generic)

makeAgendaReportLines :: AgendaReport -> [AgendaReportLine]
makeAgendaReportLines AgendaReport {..} =
  intercalate [SpaceLine] $
    filter
      (not . null)
      [goBlocks agendaReportPast, goBlocks agendaReportPresent, goBlocks agendaReportFuture]
  where
    goBlocks :: [AgendaTableBlock Text] -> [AgendaReportLine]
    goBlocks bs =
      case bs of
        [] -> []
        [b] -> goEntries $ blockEntries b
        _ -> concatMap goBlock bs
    goBlock :: AgendaTableBlock Text -> [AgendaReportLine]
    goBlock Block {..} = TitleLine blockTitle : goEntries blockEntries
    goEntries :: [AgendaEntry] -> [AgendaReportLine]
    goEntries = map EntryLine

-- TODO: this won't work at the start or the end.
-- we will want to do some lookahead.
addNowLine :: ZonedTime -> [AgendaReportLine] -> [AgendaReportLine]
addNowLine now = go
  where
    go = \case
      [] -> []
      [x] -> [x]
      (x : y : zs) ->
        case (x, y) of
          (EntryLine xe, EntryLine ye) ->
            let beforeT = agendaEntryTimestamp xe
                afterT = agendaEntryTimestamp ye
             in if isBetween beforeT now afterT then x : NowLine : y : zs else x : go (y : zs)
          _ -> x : go (y : zs)

isBetween :: Timestamp -> ZonedTime -> Timestamp -> Bool
isBetween before now after =
  beforeUTC <= nowUTC && nowUTC <= afterUTC
  where
    tz = zonedTimeZone now
    beforeUTC = localTimeToUTC tz $ timestampLocalTime before
    afterUTC = localTimeToUTC tz $ timestampLocalTime after
    nowUTC = zonedTimeToUTC now
