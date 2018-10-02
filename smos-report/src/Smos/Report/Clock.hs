{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Smos.Report.Clock where

import GHC.Generics (Generic)

import Data.Maybe

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Time
import Data.Tree

import Path

import Conduit

import Smos.Data

import Smos.Report.Formatting
import Smos.Report.OptParse
import Smos.Report.Streaming

clock :: Settings -> IO ()
clock Settings {..} = do
    tups <-
        sourceToList $
        sourceFilesInNonHiddenDirsRecursively setWorkDir .| filterSmosFiles .|
        parseSmosFiles setWorkDir .|
        printShouldPrint setShouldPrint
    T.putStrLn $ renderClockTable $ makeClockTable tups

data ClockTableEntry = ClockTableEntry
    { clockTableEntryFile :: Path Rel File
    , clockTableEntryHeader :: Header
    , clockTableEntryTime :: NominalDiffTime
    } deriving (Show, Eq, Generic)

makeClockTable :: [(Path Rel File, SmosFile)] -> [ClockTableEntry]
makeClockTable = concatMap $ uncurry go
  where
    go :: Path Rel File -> SmosFile -> [ClockTableEntry]
    go rf = mapMaybe go' . concatMap flatten . smosFileForest
      where
        go' :: Entry -> Maybe ClockTableEntry
        go' Entry {..} =
            let t = sumLogbookTime entryLogbook
             in if t > 0
                    then Just
                             ClockTableEntry
                                 { clockTableEntryFile = rf
                                 , clockTableEntryHeader = entryHeader
                                 , clockTableEntryTime = t
                                 }
                    else Nothing

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
        , show clockTableEntryTime
        ]
