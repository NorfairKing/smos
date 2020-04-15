{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query.Commands.Clock
  ( smosQueryClock,
  )
where

import Conduit
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.List as C
import qualified Data.Sequence as S
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time
import Data.Tree
import Data.Validity.Path ()
import qualified Data.Yaml.Builder as Yaml
import Rainbow
import Rainbox
import Smos.Query.Clock.Types
import Smos.Query.Config
import Smos.Query.Formatting
import Smos.Query.OptParse.Types
import Smos.Query.Streaming
import Smos.Report.Clock
import Smos.Report.Streaming
import Smos.Report.TimeBlock
import Text.Printf

smosQueryClock :: ClockSettings -> Q ()
smosQueryClock ClockSettings {..} = do
  now <- liftIO getZonedTime
  tups <-
    sourceToList $
      streamSmosFiles clockSetHideArchive .| parseSmosFiles .| printShouldPrint PrintWarning
        .| ( case clockSetFilter of
               Nothing -> C.map id
               Just f -> C.map (\(rp, sf) -> (,) rp (zeroOutByFilter f rp sf))
           )
        .| C.mapMaybe (uncurry (findFileTimes $ zonedTimeToUTC now))
        .| C.mapMaybe (trimFileTimes now clockSetPeriod)
  let clockTable = makeClockTable $ divideIntoClockTimeBlocks now clockSetBlock tups
  liftIO $
    case clockSetOutputFormat of
      OutputPretty ->
        putBoxLn
          $ renderClockTable clockSetReportStyle clockSetClockFormat
          $ clockTableRows clockTable
      OutputYaml -> SB.putStr $ Yaml.toByteString clockTable
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
    goB b@Block {..} = BlockTitleRow blockTitle : goFs blockEntries ++ [BlockTotalRow $ sumBlock b]
    goFs :: [ClockTableFile] -> [ClockTableRow]
    goFs = concatMap goF
    goF :: ClockTableFile -> [ClockTableRow]
    goF ClockTableFile {..} =
      FileRow clockTableFile (sumForest clockTableForest) : goHF 0 clockTableForest
      where
        goHF :: Int -> Forest ClockTableHeaderEntry -> [ClockTableRow]
        goHF l = concatMap $ goHT l
        goHT :: Int -> Tree ClockTableHeaderEntry -> [ClockTableRow]
        goHT l t@(Node ClockTableHeaderEntry {..} f) =
          EntryRow l clockTableHeaderEntryHeader clockTableHeaderEntryTime (sumTree t)
            : goHF (l + 1) f
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
renderClockTable :: ClockReportStyle -> ClockFormat -> [ClockTableRow] -> Box Vertical
renderClockTable crs fmt = tableByRows . S.fromList . map S.fromList . concatMap renderRows
  where
    renderRows :: ClockTableRow -> [[Cell]]
    renderRows ctr =
      case ctr of
        BlockTitleRow t -> [[cell $ blockTitleChunk t]]
        FileRow rp ndt ->
          [ map
              cell
              [ fore green $ rootedPathChunk rp,
                chunk "",
                chunk "",
                chunk "",
                fore green $ chunk $ renderNominalDiffTime fmt ndt
              ]
          ]
        EntryRow i h ndt ndtt ->
          case crs of
            ClockFlat ->
              [ if ndt == 0
                  then []
                  else
                    [ cell $ chunk "",
                      separator mempty 1,
                      cell $ headerChunk h,
                      cell $ chunk $ renderNominalDiffTime fmt ndt
                    ]
              ]
            ClockForest ->
              [ [ cell $ chunk "",
                  separator mempty 1,
                  cell $ chunk (T.pack $ replicate (2 * i) ' ') <> headerChunk h,
                  cell
                    $ chunk
                    $ if ndt == 0
                      then ""
                      else renderNominalDiffTime fmt ndt,
                  cell
                    $ fore brown
                    $ chunk
                    $ if ndt == ndtt
                      then ""
                      else renderNominalDiffTime fmt ndtt
                ]
              ]
        BlockTotalRow t ->
          [ map
              (cell . fore blue)
              [chunk "", chunk "", chunk "Total:", chunk $ renderNominalDiffTime fmt t],
            replicate 5 emptyCell
          ]
        AllTotalRow t ->
          [ map
              (cell . fore blue)
              [chunk "", chunk "", chunk "Total:", chunk $ renderNominalDiffTime fmt t]
          ]
    blockTitleChunk :: Text -> Chunk Text
    blockTitleChunk = fore blue . chunk
    emptyCell :: Cell
    emptyCell = cell $ chunk ""
    cell :: Chunk Text -> Cell
    cell c = mempty {_rows = S.singleton (S.singleton c), _vertical = left}

renderNominalDiffTime :: ClockFormat -> NominalDiffTime -> Text
renderNominalDiffTime fmt ndt =
  case fmt of
    ClockFormatTemporal res ->
      T.intercalate ":" $
        concat
          [ [T.pack $ printf "%5.2d" hours | res <= TemporalHoursResolution],
            [T.pack $ printf "%.2d" minutes | res <= TemporalMinutesResolution],
            [T.pack $ printf "%.2d" seconds | res <= TemporalSecondsResolution]
          ]
    ClockFormatDecimal res ->
      case res of
        DecimalHoursResolution -> T.pack $ printf "%5.0f" fractionalHours
        DecimalQuarterResolution -> T.pack $ printf "%5.2f" quarters
        DecimalResolution w -> T.pack $ printf ("%5." ++ show w ++ "f") fractionalHours
  where
    fractionalHours = realToFrac ndt / (60 * 60) :: Double
    quarters = fromIntegral (round (fractionalHours * 4) :: Int) / 4 :: Double
    totalSeconds = round ndt :: Int
    totalMinutes = totalSeconds `div` secondsInAMinute
    totalHours = totalMinutes `div` minutesInAnHour
    secondsInAMinute = 60
    minutesInAnHour = 60
    hours = totalHours
    minutes = totalMinutes - minutesInAnHour * totalHours
    seconds = totalSeconds - secondsInAMinute * totalMinutes
