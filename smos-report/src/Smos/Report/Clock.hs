{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Smos.Report.Clock where

import GHC.Generics (Generic)

import Data.Maybe

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Time
import Data.Tree
import Data.Validity
import Text.Printf

import Path

import Conduit

import Smos.Data

import Smos.Report.Formatting
import Smos.Report.OptParse
import Smos.Report.Streaming

clock :: ClockSettings -> Settings -> IO ()
clock ClockSettings {..} Settings {..} = do
    tups <-
        sourceToList $
        sourceFilesInNonHiddenDirsRecursively setWorkDir .| filterSmosFiles .|
        parseSmosFiles setWorkDir .|
        printShouldPrint setShouldPrint
    now <- getZonedTime
    T.putStrLn $ renderClockTable $ makeClockTable now clockSetPeriod tups

data ClockTableEntry = ClockTableEntry
    { clockTableEntryFile :: Path Rel File
    , clockTableEntryHeader :: Header
    , clockTableEntryTime :: NominalDiffTime
    } deriving (Show, Eq, Generic)

makeClockTable ::
       ZonedTime
    -> ClockPeriod
    -> [(Path Rel File, SmosFile)]
    -> [ClockTableEntry]
makeClockTable zt cp = concatMap $ uncurry go
  where
    go :: Path Rel File -> SmosFile -> [ClockTableEntry]
    go rf = mapMaybe go' . concatMap flatten . smosFileForest
      where
        go' :: Entry -> Maybe ClockTableEntry
        go' Entry {..} =
            let t = sumLogbookTime $ trimLogbook zt cp entryLogbook
             in if t > 0
                    then Just
                             ClockTableEntry
                                 { clockTableEntryFile = rf
                                 , clockTableEntryHeader = entryHeader
                                 , clockTableEntryTime = t
                                 }
                    else Nothing

trimLogbook :: ZonedTime -> ClockPeriod -> Logbook -> Logbook
trimLogbook now cp lb =
    case cp of
        AllTime -> lb
        Today -> trimEntries trimToToday lb
  where
    trimEntries :: (LogbookEntry -> Maybe LogbookEntry) -> Logbook -> Logbook
    trimEntries func lb_ =
        case lb_ of
            LogOpen ut les -> LogOpen ut $ mapMaybe func les
            LogClosed les -> LogClosed $ mapMaybe func les
    tz :: TimeZone
    tz = zonedTimeZone now
    today :: LocalTime
    today = zonedTimeToLocalTime now
    toLocal :: UTCTime -> LocalTime
    toLocal = utcToLocalTime tz
    fromLocal :: LocalTime -> UTCTime
    fromLocal = localTimeToUTC tz
    todayStart :: LocalTime
    todayStart = today {localTimeOfDay = midnight}
    todayEnd :: LocalTime
    todayEnd =
        today {localDay = addDays 1 $ localDay today, localTimeOfDay = midnight}
    trimToToday :: LogbookEntry -> Maybe LogbookEntry
    trimToToday = trimTo todayStart todayEnd
    trimTo :: LocalTime -> LocalTime -> LogbookEntry -> Maybe LogbookEntry
    trimTo begin end LogbookEntry {..} =
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

sumLogbookTime :: Logbook -> NominalDiffTime
sumLogbookTime lb =
    sum $
    case lb of
        (LogOpen _ es) -> map go es
        (LogClosed es) -> map go es
  where
    go :: LogbookEntry -> NominalDiffTime
    go LogbookEntry {..} = diffUTCTime logbookEntryEnd logbookEntryStart

renderClockTable :: [ClockTableEntry] -> Text
renderClockTable = T.pack . formatAsTable . map go
  where
    go :: ClockTableEntry -> [String]
    go ClockTableEntry {..} =
        [ fromRelFile clockTableEntryFile
        , T.unpack $ headerText clockTableEntryHeader
        , T.unpack $ renderNominalDiffTime clockTableEntryTime
        ]

renderNominalDiffTime :: NominalDiffTime -> Text
renderNominalDiffTime ndt =
    T.intercalate
        ":"
        [ T.pack $ printf "%5.2d" hours
        , T.pack $ printf "%.2d" minutes
        , T.pack $ printf "%.2d" seconds
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
