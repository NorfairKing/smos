{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Clock
    ( module Smos.Report.Clock
    , module Smos.Report.Clock.Types
    ) where

import GHC.Generics (Generic)

import Data.Maybe

import Data.Function
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
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
import Smos.Report.TimeBlock

data ClockTime = ClockTime
    { clockTimeFile :: RootedPath
    , clockTimeHeader :: Header
    , clockTimeEntries :: NonEmpty LogbookEntry
    } deriving (Show, Eq, Generic)

instance Validity ClockTime

findClockTimes :: RootedPath -> Entry -> Maybe ClockTime
findClockTimes rp Entry {..} =
    case entryLogbook of
        LogOpen _ es -> go' es
        LogClosed es -> go' es
  where
    go' es = do
        ne <- NE.nonEmpty es
        pure $
            ClockTime
                { clockTimeFile = rp
                , clockTimeHeader = entryHeader
                , clockTimeEntries = ne
                }

trimClockTime :: ZonedTime -> Period -> ClockTime -> Maybe ClockTime
trimClockTime zt cp ct = do
    let entries =
            mapMaybe (trimLogbookEntry zt cp) $ NE.toList $ clockTimeEntries ct
    ne <- NE.nonEmpty entries
    pure ct {clockTimeEntries = ne}

trimLogbookEntry :: ZonedTime -> Period -> LogbookEntry -> Maybe LogbookEntry
trimLogbookEntry now cp =
    case cp of
        AllTime -> pure
        Today -> trimToToday
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

type ClockTimeBlock a = Block a ClockTime

divideIntoClockTimeBlocks ::
       TimeZone -> TimeBlock -> [ClockTime] -> [ClockTimeBlock Text]
divideIntoClockTimeBlocks tz cb cts =
    case cb of
        OneBlock -> [Block {blockTitle = "All Time", blockEntries = cts}]
        DayBlock ->
            map (mapBlockTitle (T.pack . show)) $
            combineBlocksByName $
            concatMap (divideClockTimeIntoDailyBlocks tz) cts

divideClockTimeIntoDailyBlocks :: TimeZone -> ClockTime -> [ClockTimeBlock Day]
divideClockTimeIntoDailyBlocks tz =
    map (uncurry makeClockTimeBlock) . sortAndGroupCombineOrd . divideClockTime
  where
    makeClockTimeBlock :: a -> [ClockTime] -> ClockTimeBlock a
    makeClockTimeBlock n cts = Block {blockTitle = n, blockEntries = cts}
    toLocal :: UTCTime -> LocalTime
    toLocal = utcToLocalTime tz
    divideClockTime :: ClockTime -> [(Day, ClockTime)]
    divideClockTime ct =
        mapMaybe
            (\(d, es) ->
                 (,) d <$>
                 ((\ne -> ct {clockTimeEntries = ne}) <$> NE.nonEmpty es)) $
        sortAndGroupCombineOrd . concatMap divideLogbookEntry $
        clockTimeEntries ct
    divideLogbookEntry :: LogbookEntry -> [(Day, LogbookEntry)]
    divideLogbookEntry lbe@LogbookEntry {..} =
        flip mapMaybe dayRange $ \d ->
            (,) d <$>
            trimLogbookEntryTo
                tz
                (LocalTime d midnight)
                (LocalTime (addDays 1 d) midnight)
                lbe
      where
        startDay = localDay $ toLocal logbookEntryStart
        endDay = localDay $ toLocal logbookEntryEnd
        dayRange = [startDay .. endDay]

sortAndGroupCombineOrd :: Ord a => [(a, b)] -> [(a, [b])]
sortAndGroupCombineOrd = sortGroupCombine compare

sortGroupCombine :: (a -> a -> Ordering) -> [(a, b)] -> [(a, [b])]
sortGroupCombine func =
    map combine .
    groupBy ((\a1 a2 -> func a1 a2 == EQ) `on` fst) . sortBy (func `on` fst)
  where
    combine [] = error "cannot happen due to groupBy above"
    combine ts@((a, _):_) = (a, map snd ts)

type ClockTable = [ClockTableBlock]

type ClockTableBlock = Block Text ClockTableEntry

makeClockTable :: [ClockTimeBlock Text] -> [ClockTableBlock]
makeClockTable = map makeClockTableBlock

makeClockTableBlock :: ClockTimeBlock Text -> ClockTableBlock
makeClockTableBlock Block {..} =
    Block
        { blockTitle = blockTitle
        , blockEntries = map makeClockTableEntry blockEntries
        }

data ClockTableEntry = ClockTableEntry
    { clockTableEntryFile :: RootedPath
    , clockTableEntryHeader :: Header
    , clockTableEntryTime :: NominalDiffTime
    } deriving (Show, Eq, Generic)

instance Validity ClockTableEntry

makeClockTableEntry :: ClockTime -> ClockTableEntry
makeClockTableEntry ClockTime {..} =
    ClockTableEntry
        { clockTableEntryFile = clockTimeFile
        , clockTableEntryHeader = clockTimeHeader
        , clockTableEntryTime = sumLogbookEntryTime $ NE.toList clockTimeEntries
        }

sumLogbookEntryTime :: [LogbookEntry] -> NominalDiffTime
sumLogbookEntryTime = sum . map go
  where
    go :: LogbookEntry -> NominalDiffTime
    go LogbookEntry {..} = diffUTCTime logbookEntryEnd logbookEntryStart
