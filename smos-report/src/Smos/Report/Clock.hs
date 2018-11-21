{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Clock
    ( module Smos.Report.Clock
    , module Smos.Report.Clock.Types
    ) where

import Debug.Trace

import Data.Function
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time
import Data.Time.Calendar.WeekDate
import Data.Validity
import Data.Validity.Path ()

import Smos.Data

import Smos.Report.Clock.Types
import Smos.Report.Path
import Smos.Report.Period
import Smos.Report.Query
import Smos.Report.TimeBlock

-- | Reset the timers of every entry that doesn't match the filter to zero
zeroOutByFilter :: Filter -> RootedPath -> SmosFile -> SmosFile
zeroOutByFilter = undefined

findFileTimes :: UTCTime -> RootedPath -> SmosFile -> Maybe FileTimes
findFileTimes now rp (SmosFile ts) = do
    ne <- goF ts
    pure $ FileTimes {clockTimeFile = traceShowId rp, clockTimeForest = ne}
  where
    goF :: Forest Entry -> Maybe (TForest HeaderTimes)
    goF = NE.nonEmpty . mapMaybe goT
    goT :: Tree Entry -> Maybe (TTree HeaderTimes)
    goT (Node e ts_) =
        case ts_ of
            [] -> do
                hts <- headerTimesNonEmpty $ findHeaderTimes now e
                pure $ TLeaf hts
            _ -> TBranch (findHeaderTimes now e) <$> goF ts_

findHeaderTimes :: UTCTime -> Entry -> HeaderTimes []
findHeaderTimes now Entry {..} =
    case entryLogbook of
        LogOpen s es ->
            (ht $
             (LogbookEntry {logbookEntryStart = s, logbookEntryEnd = now}) : es)
        LogClosed es -> ht es
  where
    ht es =
        HeaderTimes {headerTimesHeader = entryHeader, headerTimesEntries = es}

headerTimesList :: HeaderTimes NonEmpty -> HeaderTimes []
headerTimesList hts =
    HeaderTimes
        { headerTimesHeader = headerTimesHeader hts
        , headerTimesEntries = NE.toList $ headerTimesEntries hts
        }

headerTimesNonEmpty :: HeaderTimes [] -> Maybe (HeaderTimes NonEmpty)
headerTimesNonEmpty hts = do
    ne <- NE.nonEmpty $ headerTimesEntries hts
    pure $
        HeaderTimes
            {headerTimesHeader = headerTimesHeader hts, headerTimesEntries = ne}

trimHeaderTimes :: ZonedTime -> Period -> HeaderTimes [] -> HeaderTimes []
trimHeaderTimes zt cp ht =
    let es' = mapMaybe (trimLogbookEntry zt cp) $ headerTimesEntries ht
     in ht {headerTimesEntries = es'}

trimLogbookEntry :: ZonedTime -> Period -> LogbookEntry -> Maybe LogbookEntry
trimLogbookEntry now cp =
    case cp of
        AllTime -> pure
        Today -> trimToToday
        LastWeek -> trimToLastWeek
        ThisWeek -> trimToThisWeek
  where
    tz :: TimeZone
    tz = zonedTimeZone now
    nowLocal :: LocalTime
    nowLocal = zonedTimeToLocalTime now
    today :: Day
    today = localDay nowLocal
    todayStart :: LocalTime
    todayStart = nowLocal {localTimeOfDay = midnight}
    todayEnd :: LocalTime
    todayEnd = nowLocal {localDay = addDays 1 today, localTimeOfDay = midnight}
    trimToToday :: LogbookEntry -> Maybe LogbookEntry
    trimToToday = trimLogbookEntryTo tz todayStart todayEnd
    lastWeekStart :: LocalTime
    lastWeekStart =
        let (y, wn, _) = toWeekDate today
         in LocalTime (fromWeekDate y (wn - 1) 1) midnight -- FIXME this will go wrong at the start of the year
    thisWeekStart :: LocalTime
    thisWeekStart =
        let (y, wn, _) = toWeekDate today
         in LocalTime (fromWeekDate y wn 1) midnight
    thisWeekEnd :: LocalTime
    thisWeekEnd =
        let (y, wn, _) = toWeekDate today
         in LocalTime (fromWeekDate y (wn + 1) 1) midnight -- FIXME this can wrong at the end of the year
    trimToThisWeek :: LogbookEntry -> Maybe LogbookEntry
    trimToThisWeek = trimLogbookEntryTo tz thisWeekStart thisWeekEnd
    trimToLastWeek :: LogbookEntry -> Maybe LogbookEntry
    trimToLastWeek = trimLogbookEntryTo tz lastWeekStart thisWeekStart

trimLogbookEntryTo ::
       TimeZone -> LocalTime -> LocalTime -> LogbookEntry -> Maybe LogbookEntry
trimLogbookEntryTo tz begin end LogbookEntry {..} =
    constructValid $
    LogbookEntry
        { logbookEntryStart =
              if toLocal logbookEntryStart >= begin
                  then logbookEntryStart
                  else fromLocal begin
        , logbookEntryEnd =
              if toLocal logbookEntryEnd < end
                  then logbookEntryEnd
                  else fromLocal end
        }
  where
    toLocal :: UTCTime -> LocalTime
    toLocal = utcToLocalTime tz
    fromLocal :: LocalTime -> UTCTime
    fromLocal = localTimeToUTC tz

divideIntoClockTimeBlocks ::
       TimeZone -> TimeBlock -> [FileTimes] -> [ClockTimeBlock Text]
divideIntoClockTimeBlocks tz cb cts =
    case cb of
        OneBlock -> [Block {blockTitle = "All Time", blockEntries = cts}]
        DayBlock ->
            map (mapBlockTitle (T.pack . show)) $
            combineBlocksByName $
            concatMap (divideClockTimeIntoDailyBlocks tz) cts

divideClockTimeIntoDailyBlocks :: TimeZone -> FileTimes -> [ClockTimeBlock Day]
divideClockTimeIntoDailyBlocks tz =
    map (uncurry makeClockTimeBlock) . sortAndGroupCombineOrd . divideFileTimes
  where
    makeClockTimeBlock :: a -> [FileTimes] -> ClockTimeBlock a
    makeClockTimeBlock n cts = Block {blockTitle = n, blockEntries = cts}
    toLocal :: UTCTime -> LocalTime
    toLocal = utcToLocalTime tz
    divideFileTimes :: FileTimes -> [(Day, FileTimes)]
    divideFileTimes ct = undefined
    --     mapMaybe
    --         (\(d, es) ->
    --              (,) d <$>
    --              ((\ne -> ct {clockTimeEntries = ne}) <$> NE.nonEmpty es)) $
    --     sortAndGroupCombineOrd . concatMap divideLogbookEntry $
    --     clockTimeEntries ct
    -- divideLogbookEntry :: LogbookEntry -> [(Day, LogbookEntry)]
    -- divideLogbookEntry lbe@LogbookEntry {..} =
    --     flip mapMaybe dayRange $ \d ->
    --         (,) d <$>
    --         trimLogbookEntryTo
    --             tz
    --             (LocalTime d midnight)
    --             (LocalTime (addDays 1 d) midnight)
    --             lbe
    --   where
    --     startDay = localDay $ toLocal logbookEntryStart
    --     endDay = localDay $ toLocal logbookEntryEnd
    --     dayRange = [startDay .. endDay]

sortAndGroupCombineOrd :: Ord a => [(a, b)] -> [(a, [b])]
sortAndGroupCombineOrd = sortGroupCombine compare

sortGroupCombine :: (a -> a -> Ordering) -> [(a, b)] -> [(a, [b])]
sortGroupCombine func =
    map combine .
    groupBy ((\a1 a2 -> func a1 a2 == EQ) `on` fst) . sortBy (func `on` fst)
  where
    combine [] = error "cannot happen due to groupBy above"
    combine ts@((a, _):_) = (a, map snd ts)

makeClockTable :: [ClockTimeBlock Text] -> ClockTable
makeClockTable = map makeClockTableBlock

makeClockTableBlock :: ClockTimeBlock Text -> ClockTableBlock
makeClockTableBlock Block {..} =
    Block
        { blockTitle = blockTitle
        , blockEntries = map makeClockTableFile blockEntries
        }

makeClockTableFile :: FileTimes -> ClockTableFile
makeClockTableFile FileTimes {..} =
    ClockTableFile
        { clockTableFile = clockTimeFile
        , clockTableForest = unTForest clockTimeForest
        }

unTForest :: TForest HeaderTimes -> Forest ClockTableHeaderEntry
unTForest = map unTTree . NE.toList

unTTree :: TTree HeaderTimes -> Tree ClockTableHeaderEntry
unTTree (TLeaf hts) = Node (makeClockTableHeaderEntry $ headerTimesList hts) []
unTTree (TBranch hts tf) = Node (makeClockTableHeaderEntry hts) (unTForest tf)

makeClockTableHeaderEntry :: HeaderTimes [] -> ClockTableHeaderEntry
makeClockTableHeaderEntry HeaderTimes {..} =
    ClockTableHeaderEntry
        { clockTableHeaderEntryHeader = headerTimesHeader
        , clockTableHeaderEntryTime = sumLogbookEntryTime $ headerTimesEntries
        }

sumLogbookEntryTime :: [LogbookEntry] -> NominalDiffTime
sumLogbookEntryTime = sum . map go
  where
    go :: LogbookEntry -> NominalDiffTime
    go LogbookEntry {..} = diffUTCTime logbookEntryEnd logbookEntryStart

trimFileTimes :: ZonedTime -> Period -> FileTimes -> Maybe FileTimes
trimFileTimes zt cp fts = do
    f <- goF $ clockTimeForest fts
    pure $ fts {clockTimeForest = f}
  where
    goF :: TForest HeaderTimes -> Maybe (TForest HeaderTimes)
    goF tf = NE.nonEmpty $ mapMaybe goT $ NE.toList tf
    goT :: TTree HeaderTimes -> Maybe (TTree HeaderTimes)
    goT (TLeaf hts) =
        TLeaf <$>
        (headerTimesNonEmpty $ trimHeaderTimes zt cp (headerTimesList hts))
    goT (TBranch hts tf) = TBranch (trimHeaderTimes zt cp hts) <$> goF tf
