{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Draw.Report
  ( drawReportCursor,
  )
where

import Brick.Types as B
import Brick.Widgets.Core as B
import Cursor.Brick
import Cursor.List.NonEmpty (foldNonEmptyCursor)
import Cursor.Simple.List.NonEmpty
import Data.Time
import Lens.Micro
import Path
import Smos.Actions
import Smos.Data
import Smos.Draw.Base
import Smos.Report.Filter
import Smos.Report.Formatting
import Smos.Style
import Smos.Types
import Text.Printf

drawReportCursor :: Select -> ReportCursor -> Drawer
drawReportCursor s = \case
  ReportNextActions narc -> pure $ drawNextActionReportCursor s narc
  ReportWaiting wrc -> drawWaitingReportCursor s wrc
  ReportTimestamps tsrc -> drawTimestampsReportCursor s tsrc

drawNextActionReportCursor :: Select -> NextActionReportCursor -> Widget ResourceName
drawNextActionReportCursor s NextActionReportCursor {..} =
  withHeading (str "Next Action Report") $
    vBox
      [ padAll 1 $
          viewport ResourceViewport Vertical $
            case nextActionReportCursorSelectedNextActionEntryCursors of
              Nothing -> txtWrap "Empty next action report"
              Just naecs -> verticalNonEmptyCursorTable (go NotSelected) (go s) (go NotSelected) naecs,
        ( case nextActionReportCursorSelection of
            NextActionReportFilterSelected -> withAttr selectedAttr
            NextActionReportSelected -> id
        )
          $ let ms =
                  case nextActionReportCursorSelection of
                    NextActionReportFilterSelected -> MaybeSelected
                    NextActionReportSelected -> NotSelected
             in hBox [textLineWidget "Filter:", txt " ", drawTextCursor ms nextActionReportCursorFilterBar]
      ]
  where
    go = drawNextActionEntryCursor

drawNextActionEntryCursor :: Select -> NextActionEntryCursor -> [Widget ResourceName]
drawNextActionEntryCursor s naec@NextActionEntryCursor {..} =
  let e@Entry {..} = naec ^. nextActionEntryCursorEntryL
      sel =
        ( case s of
            MaybeSelected -> forceAttr selectedAttr . visible
            NotSelected -> id
        )
   in [ drawFilePath nextActionEntryCursorFilePath,
        maybe emptyWidget drawTodoState $ entryState e,
        sel $ drawHeader entryHeader
      ]

drawWaitingReportCursor :: Select -> WaitingReportCursor -> Drawer
drawWaitingReportCursor s WaitingReportCursor {..} = do
  now <- asks zonedTimeToUTC
  let go = drawWaitingEntryCursor now
  pure $
    withHeading (str "Waiting Report") $
      padAll 1 $
        viewport ResourceViewport Vertical $
          case waitingReportCursorWaitingEntryCursors of
            Nothing -> txtWrap "Empty waiting report"
            Just wecs -> verticalNonEmptyCursorTable (go NotSelected) (go s) (go NotSelected) wecs

drawWaitingEntryCursor :: UTCTime -> Select -> WaitingEntryCursor -> [Widget ResourceName]
drawWaitingEntryCursor now s WaitingEntryCursor {..} =
  let sel =
        ( case s of
            MaybeSelected -> forceAttr selectedAttr . visible
            NotSelected -> id
        )
   in [ str $ toFilePath waitingEntryCursorFilePath,
        sel $ drawHeader $ entryHeader $ forestCursorCurrent waitingEntryCursorForestCursor,
        daysSinceWidget 7 now waitingEntryCursorTimestamp
      ]

daysSinceWidget :: Word -> UTCTime -> UTCTime -> Widget n
daysSinceWidget threshold now t = withAttr style $ str $ show i <> " days"
  where
    th1 = fromIntegral threshold :: Int
    th2 = floor ((fromIntegral threshold :: Double) / 3 * 2) :: Int
    th3 = floor ((fromIntegral threshold :: Double) / 3) :: Int
    style
      | i >= th1 = waitingReportLongWait
      | i >= th2 = waitingReportMidWait
      | i >= th3 = waitingReportShortWait
      | otherwise = waitingReportNoWait
    i = daysSince now t

drawTimestampsReportCursor :: Select -> TimestampsReportCursor -> Drawer
drawTimestampsReportCursor s TimestampsReportCursor {..} = do
  now <- ask
  tableW <- case timestampsReportCursorTimestampsEntryCursors of
    Nothing -> pure $ txtWrap "Empty timestamps report"
    Just tsecs -> do
      ws <- mapM (drawTimestampReportLine s) $ makeTimestampReportLines now tsecs
      pure $ tableWidget ws
  pure $
    withHeading (str "Agenda Report: Today") $
      padAll 1 $
        viewport ResourceViewport Vertical tableW

data TimestampsReportLine
  = ReportSelectedEntryLine TimestampsEntryCursor
  | ReportEntryLine TimestampsEntryCursor
  | ReportNowLine LocalTime
  | ReportHourLine Int

makeTimestampReportLines :: ZonedTime -> NonEmptyCursor TimestampsEntryCursor -> [TimestampsReportLine]
makeTimestampReportLines now = foldNonEmptyCursor $ \befores current afters ->
  insertReportNowLine now $ insertReportHourLines now $ concat [map ReportEntryLine befores, [ReportSelectedEntryLine current], map ReportEntryLine afters]

drawTimestampReportLine :: Select -> TimestampsReportLine -> Drawer' [Widget ResourceName]
drawTimestampReportLine s = \case
  ReportSelectedEntryLine tsec -> drawTimestampsEntryCursor s tsec
  ReportEntryLine tsec -> drawTimestampsEntryCursor NotSelected tsec
  ReportNowLine now ->
    pure $
      map
        (withAttr agendaReportNowLineAttr)
        [ str "--------------------",
          str "--------",
          str "---",
          str "---",
          str $ formatTime defaultTimeLocale "[ %H:%M:%S ]" now,
          str "---"
        ]
  ReportHourLine i ->
    pure
      [ str (printf ".......... %02d:00 ..." i),
        empty,
        empty,
        empty,
        empty,
        empty
      ]
  where
    empty = str " "

insertReportHourLines :: ZonedTime -> [TimestampsReportLine] -> [TimestampsReportLine]
insertReportHourLines now = go [8 .. 18]
  where
    ZonedTime lt _ = now
    today = localDay lt
    go hs [] = map ReportHourLine hs
    go [] es = es
    go (h : hs) (e : es) =
      let alt = timestampsReportLineLocalTime now e
          hlt = hourLineLocalTime today h
       in if alt < hlt
            then e : go (h : hs) es
            else ReportHourLine h : go hs (e : es)

timestampsReportLineLocalTime :: ZonedTime -> TimestampsReportLine -> LocalTime
timestampsReportLineLocalTime now = \case
  ReportSelectedEntryLine tec -> timestampLocalTime $ timestampsEntryCursorTimestamp tec
  ReportEntryLine tec -> timestampLocalTime $ timestampsEntryCursorTimestamp tec
  ReportNowLine lt -> lt
  ReportHourLine h -> hourLineLocalTime (localDay $ zonedTimeToLocalTime now) h

hourLineLocalTime :: Day -> Int -> LocalTime
hourLineLocalTime d h = LocalTime d (TimeOfDay h 0 0)

insertReportNowLine :: ZonedTime -> [TimestampsReportLine] -> [TimestampsReportLine]
insertReportNowLine now = go
  where
    nowL = ReportNowLine $ zonedTimeToLocalTime now
    go = \case
      [] -> [nowL]
      (x : xs) ->
        if isBefore now x
          then nowL : x : xs
          else x : go xs

isBefore :: ZonedTime -> TimestampsReportLine -> Bool
isBefore now after =
  let afterLT = timestampsReportLineLocalTime now after
      nowUTC = zonedTimeToUTC now
   in nowUTC <= localTimeToUTC (zonedTimeZone now) afterLT

drawTimestampsEntryCursor :: Select -> TimestampsEntryCursor -> Drawer' [Widget ResourceName]
drawTimestampsEntryCursor s TimestampsEntryCursor {..} = do
  let sel =
        ( case s of
            MaybeSelected -> forceAttr selectedAttr . visible
            NotSelected -> id
        )
      e = forestCursorCurrent timestampsEntryCursorForestCursor
  tsw <- drawTimestampPrettyRelative timestampsEntryCursorTimestamp
  let lt = timestampLocalTime timestampsEntryCursorTimestamp
  pure
    [ str $ show lt,
      withAttr agendaReportRelativeAttr tsw,
      drawTimestampName timestampsEntryCursorTimestampName,
      maybe (str " ") drawTodoState $ entryState e,
      sel $ drawHeader $ entryHeader e,
      str $ toFilePath timestampsEntryCursorFilePath
    ]
