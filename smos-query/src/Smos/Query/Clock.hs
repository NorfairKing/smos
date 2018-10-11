{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Clock
    ( clock
    ) where

import Data.Maybe

import qualified Data.Text as T
import Data.Text (Text)
import Data.Time
import Data.Validity.Path ()
import Text.Printf

import Rainbow

import Conduit

import Smos.Report.Clock
import Smos.Report.Path
import Smos.Report.Streaming

import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types

clock :: ClockSettings -> Q ()
clock ClockSettings {..} = do
    wd <- askWorkDir
    let filesSource =
            case clockSetFile of
                Nothing -> sourceFilesInNonHiddenDirsRecursively wd
                Just f -> yield $ Absolute f
    liftIO $ do
        tups <-
            sourceToList $
            filesSource .| filterSmosFiles .| parseSmosFiles .|
            printShouldPrint PrintWarning .|
            trimByTags clockSetTags
        now <- getZonedTime
        putTableLn $
            renderClockTable clockSetResolution $
            makeClockTable $
            divideIntoBlocks (zonedTimeZone now) clockSetBlock $
            concatMap
                (mapMaybe (trimClockTime now clockSetPeriod) .
                 uncurry findClockTimes)
                tups

renderClockTable :: ClockResolution -> [ClockTableBlock] -> Table
renderClockTable res ctbs = formatAsTable $ case ctbs of
    [] -> []
    [ctb] -> map go (clockTableBlockEntries ctb)
    _ -> concatMap goB ctbs
  where
    goB :: ClockTableBlock -> [[Chunk Text]]
    goB ClockTableBlock {..} =
        [chunk clockTableBlockName] : map go clockTableBlockEntries
    go :: ClockTableEntry -> [Chunk Text]
    go ClockTableEntry {..} =
        [ rootedPathChunk clockTableEntryFile
        , headerChunk clockTableEntryHeader
        , chunk $ renderNominalDiffTime res clockTableEntryTime
        ]

renderNominalDiffTime :: ClockResolution -> NominalDiffTime -> Text
renderNominalDiffTime res ndt =
    T.intercalate ":" $
    concat
        [ [T.pack $ printf "%5.2d" hours | res <= HoursResolution]
        , [T.pack $ printf "%.2d" minutes | res <= MinutesResolution]
        , [T.pack $ printf "%.2d" seconds | res <= SecondsResolution]
        ]
  where
    totalSeconds = round ndt :: Int
    totalMinutes = totalSeconds `div` secondsInAMinute
    totalHours = totalMinutes `div` minutesInAnHour
    secondsInAMinute = 60
    minutesInAnHour = 60
    hours = totalHours
    minutes = totalMinutes - minutesInAnHour * totalHours
    seconds = totalSeconds - secondsInAMinute * totalMinutes
