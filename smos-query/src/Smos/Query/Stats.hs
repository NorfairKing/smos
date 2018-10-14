{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Smos.Query.Stats where

import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Map as M
import Data.Map (Map)

import Conduit
import qualified Data.Conduit.Combinators as C
import Rainbow

import Smos.Data

import Smos.Report.Query
import Smos.Report.Stats
import Smos.Report.Streaming

import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types

stats :: StatsSettings -> Q ()
stats StatsSettings {..} = do
    wd <- askWorkDir
    liftIO $ do
        es <-
            sourceToList $
            sourceFilesInNonHiddenDirsRecursively wd .| filterSmosFiles .|
            parseSmosFiles .|
            printShouldPrint PrintWarning .|
            smosFileCursors .|
            C.filter (maybe (const True) filterPredicate statsSetFilter . snd) .|
            smosCursorCurrents .|
            C.map snd
        putTableLn $ renderStatsReport $ makeStatsReport es

renderStatsReport :: StatsReport -> Table
renderStatsReport StatsReport {..} =
    formatAsTable $
    concat
        [ [[fore white $ chunk "Historical states"]]
        , formatReportStates statsReportHistoricalStates
        , [[chunk ""]]
        , [[fore white $ chunk "Current states"]]
        , formatReportStates statsReportStates
        ]

formatReportStates :: Map (Maybe TodoState) Int -> [[Chunk Text]]
formatReportStates m =
    flip map (M.toList m) $ \(mts, i) ->
        [maybe (chunk "(none)") todoStateChunk mts, chunk $ T.pack $ show i]
