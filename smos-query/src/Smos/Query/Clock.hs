{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Clock
    ( clock
    ) where

import Debug.Trace

import qualified Data.Aeson as JSON
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Foldable
import Data.Maybe
import qualified Data.Sequence as S
import Data.Sequence (Seq, (<|), (><))
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time
import Data.Tree
import Data.Validity.Path ()
import qualified Data.Yaml as Yaml
import Text.Printf

import Rainbow
import Rainbox

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

import Smos.Query.Clock.Types

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
            C.mapMaybe (uncurry (findFileTimes $ zonedTimeToUTC now)) .|
            C.mapMaybe (trimFileTimes now clockSetPeriod)
        let clockTable =
                makeClockTable $
                divideIntoClockTimeBlocks (zonedTimeZone now) clockSetBlock tups
        case clockSetOutputFormat of
            OutputPretty ->
                putBoxLn $
                renderClockTable clockSetResolution $ clockTableRows clockTable
            OutputYaml -> SB.putStr $ Yaml.encode clockTable
            OutputJSON -> LB.putStr $ JSON.encode clockTable
            OutputJSONPretty -> LB.putStr $ JSON.encodePretty clockTable

clockTableRows :: ClockTable -> [ClockTableRow]
clockTableRows ctbs =
    case ctbs of
        [] -> []
        [ctb] -> goFs (blockEntries ctb) ++ allTotalRow
        _ -> goBs ctbs ++ allTotalRow
  where
    allTotalRow = [AllTotalRow $ sumTable ctbs]
    goBs :: [ClockTableBlock] -> [ClockTableRow]
    goBs = concatMap goB
    goB :: ClockTableBlock -> [ClockTableRow]
    goB b@Block {..} =
        BlockTitleRow blockTitle :
        goFs blockEntries ++ [BlockTotalRow $ sumBlock b]
    goFs :: [ClockTableFile] -> [ClockTableRow]
    goFs = concatMap goF
    goF :: ClockTableFile -> [ClockTableRow]
    goF ClockTableFile {..} = goHF 0 True clockTableForest
      where
        goHF :: Int -> Bool -> Forest ClockTableHeaderEntry -> [ClockTableRow]
        goHF l b = concatMap (uncurry $ goHT l) . zip (b : repeat False)
        goHT :: Int -> Bool -> Tree ClockTableHeaderEntry -> [ClockTableRow]
        goHT l b t@(Node ClockTableHeaderEntry {..} f) =
            EntryRow
                (if b
                     then Just clockTableFile
                     else Nothing)
                l
                clockTableHeaderEntryHeader
                clockTableHeaderEntryTime
                (sumTree t) :
            goHF (l + 1) False f
    sumTable :: ClockTable -> NominalDiffTime
    sumTable = sum . map sumBlock
    sumBlock :: ClockTableBlock -> NominalDiffTime
    sumBlock = sum . map sumFile . blockEntries
    sumFile :: ClockTableFile -> NominalDiffTime
    sumFile = sumForest . clockTableForest
    sumTree :: Tree ClockTableHeaderEntry -> NominalDiffTime
    sumTree = sum . map clockTableHeaderEntryTime . flatten
    sumForest :: Forest ClockTableHeaderEntry -> NominalDiffTime
    sumForest = sum . map sumTree

-- We want the following columns
--
-- block title
-- file name    headers and   time
--                           total time
renderClockTable :: ClockResolution -> [ClockTableRow] -> Box Vertical
renderClockTable res =
    tableByRows . S.fromList . map S.fromList . concatMap renderRows
  where
    renderRows :: ClockTableRow -> [[Cell]]
    renderRows ctr =
        case ctr of
            BlockTitleRow t -> [[cell $ blockTitleChunk t]]
            EntryRow mrp i h ndt ndtt ->
                [ [ cell $ maybe (chunk "") (fore green . rootedPathChunk) mrp
                  , separator mempty 1
                  , cell $
                    chunk (T.pack $ replicate (2 * i) ' ') <> headerChunk h
                  , cell $
                    chunk $
                    if ndt == 0
                        then ""
                        else renderNominalDiffTime res ndt
                  , cell $
                    fore brown $
                    chunk $
                    if ndt == ndtt
                        then ""
                        else renderNominalDiffTime res ndtt
                  ]
                ]
            BlockTotalRow t ->
                [ map (cell . fore blue) $
                  [ chunk ""
                  , chunk ""
                  , chunk "Total:"
                  , chunk $ renderNominalDiffTime res t
                  ]
                , replicate 5 emptyCell
                ]
            AllTotalRow t ->
                [ map (cell . fore blue) $
                  [ chunk ""
                  , chunk ""
                  , chunk "Total:"
                  , chunk $ renderNominalDiffTime res t
                  ]
                ]
    blockTitleChunk :: Text -> Chunk Text
    blockTitleChunk = fore blue . chunk
    emptyCell :: Cell
    emptyCell = cell $ chunk ""
    cell :: Chunk Text -> Cell
    cell c = mempty {_rows = S.singleton (S.singleton c), _vertical = left}
    brown = color256 166
  --   case ctbs of
  --       [] -> mempty
  --       [ctb] -> goFs $ blockEntries ctb
  --       _ -> goBs ctbs
  -- where
  --   goBs :: [ClockTableBlock] -> Seq (Seq Cell)
  --   goBs ctbs_ = concatMapSeq goB ctbs_
  --       -- concatMap goB ctbs_ ++
  --       -- [ [chunk "", chunk "", chunk ""]
  --       -- , map (fore blue) $
  --       --   [ chunk ""
  --       --   , chunk "Total:"
  --       --   , chunk $ renderNominalDiffTime res $ sumBlocks ctbs_
  --       --   ]
  --       -- ]
  --   goB :: ClockTableBlock -> Seq (Seq Cell)
  --   goB Block {..} =
  --       (S.singleton $ cell $ fore blue $ chunk blockTitle) <| goFs blockEntries
  --   goFs :: [ClockTableFile] -> Seq (Seq Cell)
  --   goFs = concatMapSeq goF
  --   goF :: ClockTableFile -> Seq (Seq Cell)
  --   goF ClockTableFile {..} =
  --       fmap ((cell $ rootedPathChunk clockTableFile) <|) $
  --       goHEs clockTableForest -- ++
  --       -- [ map (fore blue) $
  --       --   [ chunk ""
  --       --   , chunk "Total:"
  --       --   , chunk $ renderNominalDiffTime res $ sumEntries es
  --       --   ]
  --       -- ]
  --   goHEs :: Forest ClockTableHeaderEntry -> Seq (Seq Cell)
  --   goHEs f =
  --       S.singleton $
  --       S.singleton $ mempty {_rows = concatMapSeq treeRow f, _vertical = left}
  --   treeRow :: Tree ClockTableHeaderEntry -> Seq (Seq (Chunk Text))
  --   treeRow (Node ClockTableHeaderEntry {..} ts) =
  --       S.singleton
  --           (S.fromList
  --                [ headerChunk clockTableHeaderEntryHeader
  --                , fore brown $
  --                  chunk $ renderNominalDiffTime res clockTableHeaderEntryTime
  --                ])
    -- sumBlocks :: [ClockTableBlock] -> NominalDiffTime
    -- sumBlocks = sum . map (sumFiles . blockEntries)
    -- sumFiles :: [ClockTableFile] -> NominalDiffTime
    -- sumFiles = sum . map sumFile
    -- sumFile :: ClockTableFile -> NominalDiffTime
    -- sumFile = sumHeaderEntries . concatMap flatten . clockTableForest
    -- sumHeaderEntries :: [ClockTableHeaderEntry] -> NominalDiffTime
    -- sumHeaderEntries = sum . map clockTableHeaderEntryTime
    -- go :: ClockTableHeaderEntry -> [Chunk Text]
    -- go ClockTableHeaderEntry {..} =
    --     [ headerChunk clockTableHeaderEntryHeader
    --     , fore brown $
    --       chunk $ renderNominalDiffTime res clockTableHeaderEntryTime
    --     ]
  --   concatMapSeq :: (a -> Seq b) -> [a] -> Seq b
  --   concatMapSeq f = concatSeq . map f
  --   concatSeq :: [Seq a] -> Seq a
  --   concatSeq [] = S.empty
  --   concatSeq (l:ls) = l >< concatSeq ls
  --   cell :: Chunk Text -> Cell
  --   cell c = mempty {_rows = S.singleton (S.singleton c), _vertical = left}
  --   brown = color256 166

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
