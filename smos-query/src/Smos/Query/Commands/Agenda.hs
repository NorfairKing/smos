{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Agenda
  ( smosQueryAgenda,
    renderAgendaReportLines,
    AgendaReportLine (..),
    insertNowLine,
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
import Text.Printf

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
  liftIO $ putTableLn $ renderAgendaReport now $ makeAgendaReport now agendaSetPeriod agendaSetBlock tups

renderAgendaReport :: ZonedTime -> AgendaReport -> Table
renderAgendaReport now = formatAsTable . renderAgendaReportLines now . makeAgendaReportLines now

renderAgendaReportLines :: ZonedTime -> [AgendaReportLine] -> [[Chunk Text]]
renderAgendaReportLines now = map $ \case
  TitleLine t -> [fore blue $ chunk t]
  SpaceLine -> [chunk ""]
  HourLine i -> [chunk $ "... " <> T.pack (printf "%02d" i) <> ":00 ..."]
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
  | HourLine Int
  | SpaceLine
  | EntryLine AgendaEntry
  deriving (Show, Eq, Generic)

makeAgendaReportLines :: ZonedTime -> AgendaReport -> [AgendaReportLine]
makeAgendaReportLines now AgendaReport {..} =
  intercalate [SpaceLine] $
    filter
      (not . null)
      [goBlocks agendaReportPast, makeAgendaTodayReportLines now agendaReportPresent, goBlocks agendaReportFuture]
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

makeAgendaTodayReportLines :: ZonedTime -> AgendaTodayReport -> [AgendaReportLine]
makeAgendaTodayReportLines now AgendaTodayReport {..} =
  insertNowLine now $ insertHourLines now agendaTodayReportEntries

insertHourLines :: ZonedTime -> [AgendaEntry] -> [AgendaReportLine]
insertHourLines now = go [0 .. 24]
  where
    ZonedTime lt _ = now
    today = localDay lt
    go hs [] = map HourLine hs
    go [] es = map EntryLine es
    go (h : hs) (e : es) =
      let alt = agendaEntryLocalTime e
          hlt = hourLineLocalTime today h
       in if alt <= hlt
            then EntryLine e : go (h : hs) es
            else HourLine h : go hs (e : es)

insertNowLine :: ZonedTime -> [AgendaReportLine] -> [AgendaReportLine]
insertNowLine now = go
  where
    go = \case
      [] -> [NowLine]
      [x] ->
        if isBefore now x
          then [NowLine, x]
          else [x, NowLine]
      (x : y : zs) ->
        if isBetween x now y
          then x : NowLine : y : zs
          else x : go (y : zs)

isBefore :: ZonedTime -> AgendaReportLine -> Bool
isBefore now after =
  let ZonedTime lt tz = now
      today = localDay lt
      mAfterLT = agendaReportLineLocalTime today after
      nowUTC = zonedTimeToUTC now
   in case mAfterLT of
        Just afterLT ->
          nowUTC <= localTimeToUTC tz afterLT
        _ -> False

isBetween :: AgendaReportLine -> ZonedTime -> AgendaReportLine -> Bool
isBetween before now after =
  let ZonedTime lt tz = now
      today = localDay lt
      mBeforeLT = agendaReportLineLocalTime today before
      mAfterLT = agendaReportLineLocalTime today after
      nowUTC = zonedTimeToUTC now
   in case (mBeforeLT, mAfterLT) of
        (Just beforeLT, Just afterLT) ->
          localTimeToUTC tz beforeLT <= nowUTC
            && nowUTC <= localTimeToUTC tz afterLT
        (_, _) -> False

agendaReportLineLocalTime :: Day -> AgendaReportLine -> Maybe LocalTime
agendaReportLineLocalTime d = \case
  TitleLine _ -> Nothing
  NowLine -> Nothing
  SpaceLine -> Nothing
  HourLine i -> Just $ hourLineLocalTime d i
  EntryLine ae -> Just $ agendaEntryLocalTime ae

agendaEntryLocalTime :: AgendaEntry -> LocalTime
agendaEntryLocalTime = timestampLocalTime . agendaEntryTimestamp

hourLineLocalTime :: Day -> Int -> LocalTime
hourLineLocalTime d h = LocalTime d (TimeOfDay h 0 0)
