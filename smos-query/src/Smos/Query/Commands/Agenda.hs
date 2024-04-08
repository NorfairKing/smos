{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Agenda
  ( smosQueryAgenda,
    renderAgendaReport,
    renderAgendaReportLines,
    AgendaReportLine (..),
    insertNowLine,
  )
where

import Conduit
import qualified Data.Text as T
import Smos.Query.Commands.Import
import Smos.Report.Agenda
import Smos.Report.TimeBlock
import Text.Printf

smosQueryAgenda :: AgendaSettings -> Q ()
smosQueryAgenda AgendaSettings {..} = do
  zone <- liftIO loadLocalTZ
  now <- liftIO getCurrentTime
  let today = localDay (utcToLocalTimeTZ zone now)
  dc <- asks envDirectorySettings
  sp <- getShouldPrint
  report <-
    produceAgendaReport
      today
      agendaSetPeriod
      agendaSetBlock
      agendaSetHideArchive
      sp
      agendaSetHistoricity
      agendaSetFilter
      dc

  colourSettings <- asks envColourSettings
  outputChunks $ renderAgendaReport colourSettings zone now report

renderAgendaReport :: ColourSettings -> TZ -> UTCTime -> AgendaReport -> [Chunk]
renderAgendaReport colourSettings zone now =
  formatAsBicolourTable colourSettings
    . renderAgendaReportLines zone now
    . makeAgendaReportLines zone now

renderAgendaReportLines :: TZ -> UTCTime -> [AgendaReportLine] -> [[Chunk]]
renderAgendaReportLines zone now = map $ \case
  TitleLine t -> [fore blue $ chunk t]
  SpaceLine -> [chunk ""]
  TodayLine -> [fore white $ chunk $ T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %A" (utcToLocalTimeTZ zone now)]
  HourLine i -> [chunk $ T.pack $ printf ".......... %02d:00 ..." i]
  NowLine ->
    map
      (fore magenta . chunk . T.pack)
      [ "--------------------",
        "--------",
        "---",
        "---",
        formatTime defaultTimeLocale "[ %H:%M:%S ]" (utcToLocalTimeTZ zone now),
        "---"
      ]
  EntryLine ae -> formatAgendaEntry zone now ae

data AgendaReportLine
  = TitleLine Text
  | NowLine
  | HourLine Int
  | TodayLine
  | SpaceLine
  | EntryLine AgendaEntry

makeAgendaReportLines :: TZ -> UTCTime -> AgendaReport -> [AgendaReportLine]
makeAgendaReportLines zone now AgendaReport {..} =
  intercalate [SpaceLine] $
    filter
      (not . null)
      [ goBlocks agendaReportPast,
        maybe [] (TodayLine :) (makeAgendaTodayReportLines zone now agendaReportPresent),
        goBlocks agendaReportFuture
      ]
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

makeAgendaTodayReportLines :: TZ -> UTCTime -> AgendaTodayReport -> Maybe [AgendaReportLine]
makeAgendaTodayReportLines zone now AgendaTodayReport {..} =
  if null agendaTodayReportEntries
    then Nothing
    else Just $ insertNowLine zone now $ insertHourLines zone now agendaTodayReportEntries

insertHourLines :: TZ -> UTCTime -> [AgendaEntry] -> [AgendaReportLine]
insertHourLines zone now = go [8 .. 18]
  where
    go hs [] = map HourLine hs
    go [] es = map EntryLine es
    go (h : hs) (e : es) =
      let alt = agendaEntryLocalTime e
          today = localDay (utcToLocalTimeTZ zone now)
          hlt = hourLineLocalTime today h
       in if alt < hlt
            then EntryLine e : go (h : hs) es
            else HourLine h : go hs (e : es)

insertNowLine :: TZ -> UTCTime -> [AgendaReportLine] -> [AgendaReportLine]
insertNowLine zone now = go
  where
    go = \case
      [] -> [NowLine]
      (x : xs) ->
        if isBefore zone now x
          then NowLine : x : xs
          else x : go xs

isBefore :: TZ -> UTCTime -> AgendaReportLine -> Bool
isBefore zone now after =
  let today = localDay (utcToLocalTimeTZ zone now)
      mAfterLT = agendaReportLineLocalTime today after
   in case mAfterLT of
        Just afterLT -> now <= localTimeToUTCTZ zone afterLT
        _ -> False

agendaReportLineLocalTime :: Day -> AgendaReportLine -> Maybe LocalTime
agendaReportLineLocalTime d = \case
  TitleLine _ -> Nothing
  NowLine -> Nothing
  SpaceLine -> Nothing
  TodayLine -> Just $ LocalTime d midnight
  HourLine i -> Just $ hourLineLocalTime d i
  EntryLine ae -> Just $ agendaEntryLocalTime ae

agendaEntryLocalTime :: AgendaEntry -> LocalTime
agendaEntryLocalTime = timestampLocalTime . agendaEntryTimestamp

hourLineLocalTime :: Day -> Int -> LocalTime
hourLineLocalTime d h = LocalTime d (TimeOfDay h 0 0)
