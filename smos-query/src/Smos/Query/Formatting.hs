{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Formatting
  ( module Smos.Query.Formatting,
    module Text.Colour.Layout,
    module Text.Colour,
  )
where

import Data.Foldable
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Data.Time.Zones
import Path
import Smos.CLI.Colour
import Smos.Data
import Smos.Report.Agenda
import Smos.Report.Entry
import Smos.Report.Formatting
import Smos.Report.Projection
import Smos.Report.Stuck
import Smos.Report.Time
import Smos.Report.Waiting
import Text.Colour
import Text.Colour.Layout
import Text.Time.Pretty

formatAsBicolourTable :: ColourSettings -> [[Chunk]] -> [Chunk]
formatAsBicolourTable cc =
  renderTable
    . ( \t ->
          t
            { tableBackground = case colourSettingBackground cc of
                UseTableBackground tb -> Just tb
                NoTableBackground -> Nothing
            }
      )
    . table

showDaysSinceWithThreshold :: Time -> UTCTime -> UTCTime -> Chunk
showDaysSinceWithThreshold threshold = showDaysSince (floor $ timeNominalDiffTime threshold / nominalDay)

showDaysSince :: Word -> UTCTime -> UTCTime -> Chunk
showDaysSince threshold now t = fore color $ chunk $ T.pack $ show i <> " days"
  where
    th1 = fromIntegral threshold :: Int
    th2 = floor ((fromIntegral threshold :: Double) / 3 * 2) :: Int
    th3 = floor ((fromIntegral threshold :: Double) / 3) :: Int
    color
      | i >= th1 = red
      | i >= th2 = yellow
      | i >= th3 = blue
      | otherwise = green
    i = daysSince now t

formatAgendaEntry :: TZ -> UTCTime -> AgendaEntry -> [Chunk]
formatAgendaEntry zone now AgendaEntry {..} =
  let d = diffDays (localDay $ utcToLocalTimeTZ zone now) (timestampDay agendaEntryTimestamp)
      func =
        if
            | d >= 0 && agendaEntryTimestampName == "DEADLINE" -> fore red
            | d == -1 && agendaEntryTimestampName == "DEADLINE" -> fore brightRed . back black
            | d >= -10 && agendaEntryTimestampName == "DEADLINE" -> fore yellow
            | d > 0 && agendaEntryTimestampName == "SCHEDULED" -> fore red
            | d == 0 && agendaEntryTimestampName == "SCHEDULED" -> fore green
            | otherwise -> id
   in [ func $ chunk $ timestampPrettyText agendaEntryTimestamp,
        func $
          bold $
            chunk $
              T.pack $
                case agendaEntryTimestamp of
                  TimestampDay _ ->
                    renderDaysAgoAuto $ daysAgo d
                  TimestampLocalTime lt ->
                    renderTimeAgoAuto $
                      timeAgo $
                        diffUTCTime
                          now
                          (localTimeToUTCTZ zone lt),
        timestampNameChunk agendaEntryTimestampName,
        mTodoStateChunk agendaEntryTodoState,
        headerChunk agendaEntryHeader,
        func $ pathChunk agendaEntryFilePath
      ]

formatWaitingEntry :: Time -> UTCTime -> WaitingEntry -> [Chunk]
formatWaitingEntry threshold now WaitingEntry {..} =
  [ pathChunk waitingEntryFilePath,
    headerChunk waitingEntryHeader,
    showDaysSinceWithThreshold (fromMaybe threshold waitingEntryThreshold) now waitingEntryTimestamp,
    maybe (chunk "") timeChunk waitingEntryThreshold
  ]

formatStuckReportEntry :: Time -> UTCTime -> StuckReportEntry -> [Chunk]
formatStuckReportEntry threshold now StuckReportEntry {..} =
  [ pathChunk stuckReportEntryFilePath,
    mTodoStateChunk stuckReportEntryState,
    headerChunk stuckReportEntryHeader,
    maybe
      (chunk "")
      ( \ts -> if ts > now then "future" else showDaysSinceWithThreshold threshold now ts
      )
      stuckReportEntryLatestChange
  ]

pathChunk :: Path b t -> Chunk
pathChunk = chunk . T.pack . toFilePath

renderEntryReport :: ColourSettings -> EntryReport -> [Chunk]
renderEntryReport cc EntryReport {..} =
  formatAsBicolourTable cc $
    map renderProjectionHeader (toList entryReportHeaders) :
    map (renderProjectees . toList) entryReportCells

renderProjectionHeader :: Projection -> Chunk
renderProjectionHeader p =
  underline $
    case p of
      OntoFile -> chunk "file"
      OntoHeader -> chunk "header"
      OntoProperty pn -> chunk $ propertyNameText pn
      OntoTag t -> chunk $ tagText t
      OntoState -> chunk "state"
      OntoTimestamp tn -> chunk $ timestampNameText tn
      OntoAncestor p' -> renderProjectionHeader p'

renderProjectees :: [Projectee] -> [Chunk]
renderProjectees = map projecteeChunk

projecteeChunk :: Projectee -> Chunk
projecteeChunk p =
  case p of
    FileProjection rp -> pathChunk rp
    HeaderProjection h -> headerChunk h
    StateProjection s -> mTodoStateChunk s
    TagProjection mt -> maybe (chunk "") tagChunk mt
    PropertyProjection pn pv -> maybe (chunk "") (propertyValueChunk pn) pv
    TimestampProjection tn tv -> maybe (chunk "") (timestampChunk tn) tv

mTodoStateChunk :: Maybe TodoState -> Chunk
mTodoStateChunk = maybe (chunk "(none)") todoStateChunk

todoStateChunk :: TodoState -> Chunk
todoStateChunk ts = (\c -> c {chunkForeground = mcolor}) . chunk . todoStateText $ ts
  where
    mcolor =
      case todoStateText ts of
        "TODO" -> Just red
        "NEXT" -> Just orange
        "STARTED" -> Just orange
        "WAITING" -> Just blue
        "READY" -> Just brown
        "DONE" -> Just green
        "CANCELLED" -> Just green
        "FAILED" -> Just brightRed
        _ -> Nothing

timestampChunk :: TimestampName -> Timestamp -> Chunk
timestampChunk tsn = (\c -> c {chunkForeground = timestampNameColor tsn}) . chunk . timestampText

timestampNameChunk :: TimestampName -> Chunk
timestampNameChunk tsn = (\c -> c {chunkForeground = timestampNameColor tsn}) . chunk . timestampNameText $ tsn

timestampNameColor :: TimestampName -> Maybe Colour
timestampNameColor tsn =
  case timestampNameText tsn of
    "BEGIN" -> Just brown
    "END" -> Just brown
    "SCHEDULED" -> Just orange
    "DEADLINE" -> Just red
    _ -> Nothing

headerChunk :: Header -> Chunk
headerChunk = fore yellow . chunk . headerText

propertyValueChunk :: PropertyName -> PropertyValue -> Chunk
propertyValueChunk pn = (\c -> c {chunkForeground = propertyNameColor pn}) . chunk . propertyValueText

propertyNameChunk :: PropertyName -> Chunk
propertyNameChunk pn = (\c -> c {chunkForeground = propertyNameColor pn}) $ chunk $ propertyNameText pn

propertyNameColor :: PropertyName -> Maybe Colour
propertyNameColor pn =
  case propertyNameText pn of
    "assignee" -> Just blue
    "brainpower" -> Just brown
    "client" -> Just green
    "estimate" -> Just green
    "goal" -> Just orange
    "timewindow" -> Just magenta
    "url" -> Just green
    _ -> Nothing

tagChunk :: Tag -> Chunk
tagChunk = fore cyan . chunk . tagText

timeChunk :: Time -> Chunk
timeChunk = chunk . renderTime

intChunk :: Int -> Chunk
intChunk = chunk . T.pack . show

orange :: Colour
orange = color256 214

brown :: Colour
brown = color256 166
