{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Formatting where

import Data.Foldable
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Data.Time.Zones
import Smos.CLI.Colour
import Smos.CLI.Formatting
import Smos.Data
import Smos.Report.Agenda
import Smos.Report.Entry
import Smos.Report.Projection
import Smos.Report.Stuck
import Smos.Report.Time
import Smos.Report.Waiting
import Text.Colour
import Text.Time.Pretty

showDaysSinceWithThreshold :: Time -> UTCTime -> UTCTime -> Chunk
showDaysSinceWithThreshold threshold = showDaysSince (floor $ timeNominalDiffTime threshold / nominalDay)

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

timeChunk :: Time -> Chunk
timeChunk = chunk . renderTime
