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
import qualified Data.Conduit.Combinators as C

import Smos.Report.Clock
import Smos.Report.Path
import Smos.Report.Query
import Smos.Report.Streaming
import Smos.Report.TimeBlock

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
            smosFileCursors .|
            C.filter (maybe (const True) filterPredicate clockSetFilter . snd) .|
            smosCursorCurrents
        now <- getZonedTime
        putTableLn $
            renderClockTable clockSetResolution $
            makeClockTable $
            divideIntoClockTimeBlocks (zonedTimeZone now) clockSetBlock $
            mapMaybe (trimClockTime now clockSetPeriod) $
            mapMaybe (uncurry findClockTimes) tups

renderClockTable :: ClockResolution -> [ClockTableBlock] -> Table
renderClockTable res ctbs =
    formatAsTable $
    case ctbs of
        [] -> []
        [ctb] -> goEs $ blockEntries ctb
        _ -> concatMap goB ctbs
  where
    goB :: ClockTableBlock -> [[Chunk Text]]
    goB Block {..} = [fore blue $ chunk blockTitle] : goEs blockEntries
    goEs es =
        map go es ++
        [ map (fore blue) $
          [ chunk ""
          , chunk "Total:"
          , chunk $ renderNominalDiffTime res $ sum $ map clockTableEntryTime es
          ]
        ]
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
