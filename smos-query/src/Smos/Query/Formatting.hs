{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Formatting where

import qualified Data.ByteString as SB
import Data.Foldable
import qualified Data.Sequence as S
import Data.Sequence (Seq)
import qualified Data.Text as T
import Data.Time
import Path
import Rainbow
import Rainbox as Box
import Smos.Data
import Smos.Report.Agenda
import Smos.Report.Entry
import Smos.Report.Formatting
import Smos.Report.Path
import Smos.Report.Projection
import Text.Time.Pretty

type Table = Seq Chunk

formatAsTable :: [[Chunk]] -> Seq Chunk
formatAsTable =
  Box.render
    . tableByRows
    . S.fromList
    . map (S.intersperse (separator mempty 1) . S.fromList . map mkCell)

mkCell :: Chunk -> Cell
mkCell c = Cell (S.singleton (S.singleton c)) center left mempty

putTableLn :: Seq Chunk -> IO ()
putTableLn myChunks = do
  printer <- byteStringMakerFromEnvironment
  mapM_ SB.putStr $ chunksToByteStrings printer $ toList myChunks

putBoxLn :: Orientation a => Box a -> IO ()
putBoxLn box = do
  printer <- byteStringMakerFromEnvironment
  mapM_ SB.putStr $ chunksToByteStrings printer $ toList $ Box.render box

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

formatAgendaEntry :: ZonedTime -> AgendaEntry -> [Chunk]
formatAgendaEntry now AgendaEntry {..} =
  let tz = zonedTimeZone now
      d = diffDays (timestampDay agendaEntryTimestamp) (localDay $ zonedTimeToLocalTime now)
      func =
        if  | d <= 0 && agendaEntryTimestampName == "DEADLINE" -> fore red
            | d == 1 && agendaEntryTimestampName == "DEADLINE" -> fore brightRed . back black
            | d <= 10 && agendaEntryTimestampName == "DEADLINE" -> fore yellow
            | d < 0 && agendaEntryTimestampName == "SCHEDULED" -> fore red
            | d == 0 && agendaEntryTimestampName == "SCHEDULED" -> fore green
            | otherwise -> id
   in [ func $ chunk $ timestampPrettyText agendaEntryTimestamp,
        func $ bold $ chunk $ T.pack $ renderTimeAgoAuto $ timeAgo $
          diffUTCTime
            (zonedTimeToUTC now)
            (localTimeToUTC tz $ timestampLocalTime agendaEntryTimestamp),
        timestampNameChunk agendaEntryTimestampName,
        maybe (chunk "") todoStateChunk agendaEntryTodoState,
        headerChunk agendaEntryHeader,
        func $ pathChunk agendaEntryFilePath
      ]

rootedPathChunk :: RootedPath -> Chunk
rootedPathChunk rp =
  chunk
    $ T.pack
    $ case rp of
      Relative _ rf -> fromRelFile rf
      Absolute af -> fromAbsFile af

pathChunk :: Path b t -> Chunk
pathChunk = chunk . T.pack . toFilePath

renderEntryReport :: EntryReport -> Table
renderEntryReport EntryReport {..} =
  formatAsTable $
    map renderProjectionHeader (toList entryReportHeaders)
      : map (renderProjectees . toList) entryReportCells

renderProjectionHeader :: Projection -> Chunk
renderProjectionHeader p =
  underline $
    case p of
      OntoFile -> chunk "file"
      OntoHeader -> chunk "header"
      OntoProperty pn -> chunk $ propertyNameText pn
      OntoTag t -> chunk $ tagText t
      OntoState -> chunk "state"
      OntoAncestor p' -> renderProjectionHeader p'

renderProjectees :: [Projectee] -> [Chunk]
renderProjectees = map projecteeChunk

projecteeChunk :: Projectee -> Chunk
projecteeChunk p =
  case p of
    FileProjection rp -> pathChunk rp
    HeaderProjection h -> headerChunk h
    StateProjection s -> maybe (chunk "") todoStateChunk s
    TagProjection mt -> maybe (chunk "") tagChunk mt
    PropertyProjection pn pv -> maybe (chunk "") (propertyValueChunk pn) pv

mTodoStateChunk :: Maybe TodoState -> Chunk
mTodoStateChunk = maybe (chunk "(none)") todoStateChunk

todoStateChunk :: TodoState -> Chunk
todoStateChunk ts = fore color . chunk . todoStateText $ ts
  where
    color =
      case todoStateText ts of
        "TODO" -> red
        "NEXT" -> orange
        "STARTED" -> orange
        "WAITING" -> blue
        "READY" -> brown
        "DONE" -> green
        "CANCELLED" -> green
        "FAILED" -> brightRed
        _ -> mempty

timestampNameChunk :: TimestampName -> Chunk
timestampNameChunk tsn = fore color . chunk . timestampNameText $ tsn
  where
    color =
      case timestampNameText tsn of
        "BEGIN" -> brown
        "END" -> brown
        "SCHEDULED" -> orange
        "DEADLINE" -> red
        _ -> mempty

headerChunk :: Header -> Chunk
headerChunk = fore yellow . chunk . headerText

propertyValueChunk :: PropertyName -> PropertyValue -> Chunk
propertyValueChunk pn = fore (propertyNameColor pn) . chunk . propertyValueText

propertyNameChunk :: PropertyName -> Chunk
propertyNameChunk pn = fore (propertyNameColor pn) $ chunk $ propertyNameText pn

propertyNameColor :: PropertyName -> Radiant
propertyNameColor pn =
  case propertyNameText pn of
    "timewindow" -> magenta
    "client" -> green
    "brainpower" -> brown
    _ -> mempty

tagChunk :: Tag -> Chunk
tagChunk = fore cyan . chunk . tagText

intChunk :: Int -> Chunk
intChunk = chunk . T.pack . show

orange :: Radiant
orange = color256 214

brown :: Radiant
brown = color256 166
