{-# LANGUAGE OverloadedStrings #-}

module Smos.CLI.Formatting where

import qualified Data.Text as T
import Data.Time
import Path
import Smos.CLI.Colour
import Smos.Data
import Text.Colour
import Text.Colour.Layout

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
    . map (map pure)

daysSince :: UTCTime -> UTCTime -> Int
daysSince now t = i
  where
    i = diffInDays now t :: Int
    diffInDays :: UTCTime -> UTCTime -> Int
    diffInDays t1 t2 = floor $ diffUTCTime t1 t2 / nominalDay

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

pathChunk :: Path b t -> Chunk
pathChunk = chunk . T.pack . toFilePath

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
    "AFTER" -> Just blue
    "BEGIN" -> Just brown
    "DEADLINE" -> Just red
    "END" -> Just brown
    "SCHEDULED" -> Just orange
    _ -> Nothing

headerChunk :: Header -> Chunk
headerChunk = fore yellow . chunk . headerText

propertyValueChunk :: PropertyName -> PropertyValue -> Chunk
propertyValueChunk pn = (\c -> c {chunkForeground = propertyNameColor pn}) . chunk . propertyValueText

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

intChunk :: Int -> Chunk
intChunk = chunk . T.pack . show

orange :: Colour
orange = color256 214

brown :: Colour
brown = color256 166
