{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Clock
    ( clock
    ) where

import Debug.Trace

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time
import Data.Tree
import Data.Validity.Path ()
import qualified Data.Yaml as Yaml
import Text.Printf

import Rainbow

import Conduit
import qualified Data.Conduit.List as C

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
        now <- getZonedTime
        tups <-
            sourceToList $
            filesSource .| filterSmosFiles .| parseSmosFiles .|
            printShouldPrint PrintWarning .|
            (case clockSetFilter of
                 Nothing -> C.map id
                 Just f -> C.map (\(rp, sf) -> (,) rp (zeroOutByFilter f rp sf))) .|
            C.mapMaybe
                (uncurry (findFileTimes $ zonedTimeToUTC now)) .|
            C.mapMaybe (trimFileTimes now clockSetPeriod)
        let clockTable =
                makeClockTable $
                divideIntoClockTimeBlocks (zonedTimeZone now) clockSetBlock tups
        case clockSetOutputFormat of
            OutputPretty ->
                putTableLn $ renderClockTable clockSetResolution clockTable
            OutputYaml -> SB.putStr $ Yaml.encode clockTable
            OutputJSON -> LB.putStr $ JSON.encode clockTable
            OutputJSONPretty -> LB.putStr $ JSON.encodePretty clockTable

renderClockTable :: ClockResolution -> ClockTable -> Table
renderClockTable res ctbs =
    formatAsTable $
    case ctbs of
        [] -> []
        [ctb] -> goFs $ blockEntries ctb
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
    goB Block {..} = [fore blue $ chunk blockTitle] : goFs blockEntries
    goFs :: [ClockTableFile] -> [[Chunk Text]]
    goFs = concatMap goF
    goF :: ClockTableFile -> [[Chunk Text]]
    goF ClockTableFile {..} =
        [rootedPathChunk clockTableFile] : goHEs clockTableForest -- ++
        -- [ map (fore blue) $
        --   [ chunk ""
        --   , chunk "Total:"
        --   , chunk $ renderNominalDiffTime res $ sumEntries es
        --   ]
        -- ]
    goHEs :: Forest ClockTableHeaderEntry -> [[Chunk Text]]
    goHEs = undefined
    sumBlocks :: [ClockTableBlock] -> NominalDiffTime
    sumBlocks = sum . map (sumFiles . blockEntries)
    sumFiles :: [ClockTableFile] -> NominalDiffTime
    sumFiles = sum . map sumFile
    sumFile :: ClockTableFile -> NominalDiffTime
    sumFile = sumHeaderEntries . concatMap flatten . clockTableForest
    sumHeaderEntries :: [ClockTableHeaderEntry] -> NominalDiffTime
    sumHeaderEntries = sum . map clockTableHeaderEntryTime
    go :: ClockTableHeaderEntry -> [Chunk Text]
    go ClockTableHeaderEntry {..} =
        [ headerChunk clockTableHeaderEntryHeader
        , fore brown $
          chunk $ renderNominalDiffTime res clockTableHeaderEntryTime
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
