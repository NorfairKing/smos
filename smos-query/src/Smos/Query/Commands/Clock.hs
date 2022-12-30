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
import Data.Foldable
import qualified Data.Text as T
import Data.Tree
import Data.Validity.Path ()
import qualified Data.Yaml.Builder as Yaml
import Smos.Query.Clock.Types
import Smos.Query.Commands.Import
import Smos.Report.Clock
import Smos.Report.Period
import Smos.Report.TimeBlock
import Text.Printf

smosQueryClock :: ClockSettings -> Q ()
smosQueryClock ClockSettings {..} = do
  now <- liftIO getZonedTime
  let today = localDay $ zonedTimeToLocalTime now
  tups <-
    sourceToList $
      streamSmosFiles clockSetHideArchive .| streamParseSmosFiles
        .| ( case clockSetFilter of
               Nothing -> C.map id
               Just f -> C.map (\(rp, sf) -> (,) rp (zeroOutByFilter f rp sf))
           )
        .| C.mapMaybe (uncurry (findFileTimes $ zonedTimeToUTC now))
        .| C.mapMaybe (trimFileTimes (zonedTimeZone now) (periodInterval today clockSetPeriod))
  let clockTable = makeClockTable $ divideIntoClockTimeBlocks now clockSetBlock tups
  out <- asks envOutputHandle
  case clockSetOutputFormat of
    OutputPretty -> do
      colourSettings <- asks envColourSettings
      outputChunks $
        renderClockTable colourSettings clockSetReportStyle clockSetClockFormat $
          clockTableRows clockTable
    OutputYaml -> liftIO $ SB.hPutStr out $ Yaml.toByteString clockTable
    OutputJSON -> liftIO $ LB.hPutStr out $ JSON.encode clockTable
    OutputJSONPretty -> liftIO $ LB.hPutStr out $ JSON.encodePretty clockTable

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
          EntryRow l clockTableHeaderEntryHeader clockTableHeaderEntryTime (sumTree t) :
          goHF (l + 1) f
    sumTable :: ClockTable -> NominalDiffTime
    sumTable = sumDiffTimes . map sumBlock
    sumBlock :: ClockTableBlock -> NominalDiffTime
    sumBlock = sumDiffTimes . map sumFile . blockEntries
    sumFile :: ClockTableFile -> NominalDiffTime
    sumFile = sumForest . clockTableForest
    sumTree :: Tree ClockTableHeaderEntry -> NominalDiffTime
    sumTree = sumDiffTimes . map clockTableHeaderEntryTime . flatten
    sumForest :: Forest ClockTableHeaderEntry -> NominalDiffTime
    sumForest = sumDiffTimes . map sumTree

-- We want the following columns
--
-- block title
-- file name    headers and   time
--                           total time
renderClockTable :: ColourSettings -> ClockReportStyle -> ClockFormat -> [ClockTableRow] -> [Chunk]
renderClockTable colourSettings crs fmt = formatAsBicolourTable colourSettings . concatMap renderRows
  where
    renderRows :: ClockTableRow -> [[Chunk]]
    renderRows ctr =
      case ctr of
        BlockTitleRow t -> [[blockTitleChunk t]]
        FileRow rp ndt ->
          [ [ fore green $ pathChunk rp,
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
                    [ chunk "",
                      chunk " ",
                      headerChunk h,
                      chunk $ renderNominalDiffTime fmt ndt
                    ]
              ]
            ClockForest ->
              [ [ chunk "",
                  chunk " ",
                  headerChunk $ Header $ T.pack (replicate (2 * i) ' ') <> headerText h,
                  chunk $
                    if ndt == 0
                      then ""
                      else renderNominalDiffTime fmt ndt,
                  fore brown $
                    chunk $
                      if ndt == ndtt
                        then ""
                        else renderNominalDiffTime fmt ndtt
                ]
              ]
        BlockTotalRow t ->
          [ map
              (fore blue)
              [chunk "", chunk "", chunk "Total:", chunk $ renderNominalDiffTime fmt t],
            replicate 5 emptyCell
          ]
        AllTotalRow t ->
          [ map
              (fore blue)
              [chunk "", chunk "", chunk "Total:", chunk $ renderNominalDiffTime fmt t]
          ]
    blockTitleChunk :: Text -> Chunk
    blockTitleChunk = fore blue . chunk
    emptyCell :: Chunk
    emptyCell = chunk ""

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

sumDiffTimes :: [NominalDiffTime] -> NominalDiffTime
sumDiffTimes = foldl' (+) 0
