{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smos.Report.Clock
  ( module Smos.Report.Clock,
    module Smos.Report.Clock.Types,
  )
where

import Cursor.Simple.Forest
import Cursor.Simple.Tree
import Data.Function
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Time
import Data.Time.Zones
import Data.Validity
import Data.Validity.Path ()
import Lens.Micro
import Path
import Smos.Data
import Smos.Directory.Streaming
import Smos.Report.Clock.Types
import Smos.Report.Filter
import Smos.Report.Period
import Smos.Report.TimeBlock

-- | Reset the timers of every entry that doesn't match the filter to zero
zeroOutByFilter :: EntryFilter -> Path Rel File -> SmosFile -> SmosFile
zeroOutByFilter f rp sf =
  let cursors = forestCursors $ smosFileForest sf
   in sf {smosFileForest = map (fmap go) cursors}
  where
    go :: ForestCursor Entry -> Entry
    go fc =
      ( if filterPredicate f (rp, fc)
          then id
          else zeroOutEntry
      )
        (fc ^. (forestCursorSelectedTreeL . treeCursorCurrentL))

zeroOutEntry :: Entry -> Entry
zeroOutEntry e = e {entryLogbook = emptyLogbook}

findFileTimes :: UTCTime -> Path Rel File -> SmosFile -> Maybe FileTimes
findFileTimes now rp sf = do
  ne <- goF (smosFileForest sf)
  pure $ FileTimes {clockTimeFile = rp, clockTimeForest = ne}
  where
    goF :: Forest Entry -> Maybe (TForest HeaderTimes)
    goF = NE.nonEmpty . mapMaybe goT
    goT :: Tree Entry -> Maybe (TTree HeaderTimes)
    goT (Node e ts_) =
      case goF ts_ of
        Nothing -> do
          hts <- headerTimesNonEmpty $ findHeaderTimes now e
          pure $ TLeaf hts
        Just f -> pure $ TBranch (findHeaderTimes now e) f

findHeaderTimes :: UTCTime -> Entry -> HeaderTimes []
findHeaderTimes now Entry {..} =
  case entryLogbook of
    LogOpen s es -> ht $ (LogbookEntry {logbookEntryStart = s, logbookEntryEnd = now}) : es
    LogClosed es -> ht es
  where
    ht es = HeaderTimes {headerTimesHeader = entryHeader, headerTimesEntries = es}

headerTimesList :: HeaderTimes NonEmpty -> HeaderTimes []
headerTimesList hts =
  HeaderTimes
    { headerTimesHeader = headerTimesHeader hts,
      headerTimesEntries = NE.toList $ headerTimesEntries hts
    }

headerTimesNonEmpty :: HeaderTimes [] -> Maybe (HeaderTimes NonEmpty)
headerTimesNonEmpty hts = do
  ne <- NE.nonEmpty $ headerTimesEntries hts
  pure $ HeaderTimes {headerTimesHeader = headerTimesHeader hts, headerTimesEntries = ne}

trimHeaderTimes :: TZ -> Interval -> HeaderTimes [] -> HeaderTimes []
trimHeaderTimes zone interval ht =
  let es' = mapMaybe (trimLogbookEntryToInterval zone interval) $ headerTimesEntries ht
   in ht {headerTimesEntries = es'}

trimLogbookEntryToInterval :: TZ -> Interval -> LogbookEntry -> Maybe LogbookEntry
trimLogbookEntryToInterval zone interval LogbookEntry {..} =
  let (mBegin, mEnd) = intervalTuple interval
   in constructValid $
        LogbookEntry
          { logbookEntryStart = case mBegin of
              Nothing -> logbookEntryStart
              Just begin ->
                if begin <= toLocal logbookEntryStart
                  then logbookEntryStart
                  else fromLocal begin,
            logbookEntryEnd = case mEnd of
              Nothing -> logbookEntryEnd
              Just end ->
                if toLocal logbookEntryEnd < end
                  then logbookEntryEnd
                  else fromLocal end
          }
  where
    toLocal :: UTCTime -> Day
    toLocal = localDay . utcToLocalTimeTZ zone
    fromLocal :: Day -> UTCTime
    fromLocal d = localTimeToUTCTZ zone (LocalTime d midnight)

divideIntoClockTimeBlocks :: TZ -> TimeBlock -> [FileTimes] -> [ClockTimeBlock Text]
divideIntoClockTimeBlocks zone cb cts =
  case cb of
    OneBlock -> [Block {blockTitle = "All Time", blockEntries = cts}]
    YearBlock -> divideClockTimeIntoTimeBlocks formatYearTitle dayYear yearInterval
    MonthBlock -> divideClockTimeIntoTimeBlocks formatMonthTitle dayMonth monthInterval
    WeekBlock -> divideClockTimeIntoTimeBlocks formatWeekTitle dayWeek weekInterval
    DayBlock -> divideClockTimeIntoTimeBlocks formatDayTitle id dayInterval
  where
    divideClockTimeIntoTimeBlocks ::
      (Ord t, Enum t) => (t -> Text) -> (Day -> t) -> (t -> Interval) -> [ClockTimeBlock Text]
    divideClockTimeIntoTimeBlocks format fromDay toInterval =
      map (mapBlockTitle format) $
        combineBlocksByName $
          concatMap
            ( divideClockTimeIntoBlocks
                zone
                (fromDay . localDay . utcToLocalTimeTZ zone)
                toInterval
            )
            cts

divideClockTimeIntoBlocks ::
  forall t.
  (Enum t, Ord t) =>
  TZ ->
  (UTCTime -> t) ->
  (t -> Interval) ->
  FileTimes ->
  [ClockTimeBlock t]
divideClockTimeIntoBlocks zone func toInterval =
  map (uncurry makeClockTimeBlock) . sortAndGroupCombineOrd . divideFileTimes
  where
    makeClockTimeBlock :: a -> [FileTimes] -> ClockTimeBlock a
    makeClockTimeBlock n cts = Block {blockTitle = n, blockEntries = cts}
    divideFileTimes :: FileTimes -> [(t, FileTimes)]
    divideFileTimes fts =
      mapMaybe
        (\d -> (,) d <$> trimFileTimes zone (toInterval d) fts)
        (S.toList $ fileTimesDays fts)
    fileTimesDays :: FileTimes -> Set t
    fileTimesDays = goTF . clockTimeForest
      where
        goTF :: TForest HeaderTimes -> Set t
        goTF = S.unions . map goTT . NE.toList
        goTT :: TTree HeaderTimes -> Set t
        goTT (TLeaf hts) = goHT $ headerTimesList hts
        goTT (TBranch hts tf) = goHT hts `S.union` goTF tf
        goHT :: HeaderTimes [] -> Set t
        goHT = S.unions . map logbookEntryDays . headerTimesEntries
        logbookEntryDays :: LogbookEntry -> Set t
        logbookEntryDays LogbookEntry {..} =
          S.fromList [func logbookEntryStart .. func logbookEntryEnd]

sortAndGroupCombineOrd :: (Ord a) => [(a, b)] -> [(a, [b])]
sortAndGroupCombineOrd = sortGroupCombine compare

sortGroupCombine :: (a -> a -> Ordering) -> [(a, b)] -> [(a, [b])]
sortGroupCombine func =
  map combine . groupBy ((\a1 a2 -> func a1 a2 == EQ) `on` fst) . sortBy (func `on` fst)
  where
    combine [] = error "cannot happen due to groupBy above"
    combine ts@((a, _) : _) = (a, map snd ts)

makeClockTable :: [ClockTimeBlock Text] -> ClockTable
makeClockTable = map makeClockTableBlock

makeClockTableBlock :: ClockTimeBlock Text -> ClockTableBlock
makeClockTableBlock Block {..} =
  Block {blockTitle = blockTitle, blockEntries = map makeClockTableFile blockEntries}

makeClockTableFile :: FileTimes -> ClockTableFile
makeClockTableFile FileTimes {..} =
  ClockTableFile {clockTableFile = clockTimeFile, clockTableForest = unTForest clockTimeForest}

unTForest :: TForest HeaderTimes -> Forest ClockTableHeaderEntry
unTForest = map unTTree . NE.toList

unTTree :: TTree HeaderTimes -> Tree ClockTableHeaderEntry
unTTree (TLeaf hts) = Node (makeClockTableHeaderEntry $ headerTimesList hts) []
unTTree (TBranch hts tf) = Node (makeClockTableHeaderEntry hts) (unTForest tf)

makeClockTableHeaderEntry :: HeaderTimes [] -> ClockTableHeaderEntry
makeClockTableHeaderEntry HeaderTimes {..} =
  ClockTableHeaderEntry
    { clockTableHeaderEntryHeader = headerTimesHeader,
      clockTableHeaderEntryTime = sumLogbookEntryTime headerTimesEntries
    }

sumLogbookEntryTime :: [LogbookEntry] -> NominalDiffTime
sumLogbookEntryTime = foldl' (+) 0 . map go
  where
    go :: LogbookEntry -> NominalDiffTime
    go LogbookEntry {..} = diffUTCTime logbookEntryEnd logbookEntryStart

trimFileTimes :: TZ -> Interval -> FileTimes -> Maybe FileTimes
trimFileTimes zone interval fts = do
  f <- goF $ clockTimeForest fts
  pure $ fts {clockTimeForest = f}
  where
    goF :: TForest HeaderTimes -> Maybe (TForest HeaderTimes)
    goF tf = NE.nonEmpty $ mapMaybe goT $ NE.toList tf
    goT :: TTree HeaderTimes -> Maybe (TTree HeaderTimes)
    goT (TLeaf hts) = TLeaf <$> headerTimesNonEmpty (trimHeaderTimes zone interval (headerTimesList hts))
    goT (TBranch hts tf) =
      case goF tf of
        Nothing -> TLeaf <$> headerTimesNonEmpty (trimHeaderTimes zone interval hts)
        Just f -> pure $ TBranch (trimHeaderTimes zone interval hts) f
