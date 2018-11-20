{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Clock
    ( clock
    ) where

import Data.Maybe

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode.Pretty as JSON
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Validity.Path ()
import qualified Data.Yaml as Yaml
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
        let clockTable =
                makeClockTable $
                divideIntoClockTimeBlocks (zonedTimeZone now) clockSetBlock $
                mapMaybe (trimClockTime now clockSetPeriod) $
                mapMaybe (uncurry (findClockTimes $ zonedTimeToUTC now)) tups
        case clockSetOutputFormat of
            OutputPretty ->
                putTableLn $ renderClockTable clockSetResolution clockTable
            OutputYaml -> SB.putStr $ Yaml.encode clockTable
            OutputJSON -> LB.putStr $ JSON.encode clockTable
            OutputJSONPretty -> LB.putStr $ JSON.encodePretty clockTable

renderClockTable :: ClockResolution -> [ClockTableBlock] -> Table
renderClockTable res ctbs =
    formatAsTable $
    case ctbs of
        [] -> []
        [ctb] -> goEs $ blockEntries ctb
        _ -> goBs ctbs
  where
    goBs :: [ClockTableBlock] -> [[Chunk Text]]
    goBs ctbs_ =
        concatMap goB ctbs_ ++
        [ [chunk "", chunk "", chunk ""]
        , map (fore blue) $
          [ chunk ""
          , chunk "Total:"
          , chunk $ renderNominalDiffTime res $ sumBlocks ctbs_
          ]
        ]
    goB :: ClockTableBlock -> [[Chunk Text]]
    goB Block {..} = [fore blue $ chunk blockTitle] : goEs blockEntries
    goEs es =
        map go es ++
        [ map (fore blue) $
          [ chunk ""
          , chunk "Total:"
          , chunk $ renderNominalDiffTime res $ sumEntries es
          ]
        ]
    sumBlocks :: [ClockTableBlock] -> NominalDiffTime
    sumBlocks = sum . map (sumEntries . blockEntries)
    sumEntries :: [ClockTableEntry] -> NominalDiffTime
    sumEntries = sum . map clockTableEntryTime
    go :: ClockTableEntry -> [Chunk Text]
    go ClockTableEntry {..} =
        [ rootedPathChunk clockTableEntryFile
        , headerChunk clockTableEntryHeader
        , fore brown $ chunk $ renderNominalDiffTime res clockTableEntryTime
        ]
    brown = color256 166

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
