{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Clock.Types where

import GHC.Generics (Generic)

import Data.Aeson
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Data.Time
import Data.Validity
import Data.Validity.Path ()

import Smos.Data

import Smos.Report.Path
import Smos.Report.TimeBlock

-- Note: the order of these constructors matters
data ClockResolution
    = SecondsResolution
    | MinutesResolution
    | HoursResolution
    deriving (Show, Eq, Ord, Generic)

instance Validity ClockResolution

instance ToJSON ClockResolution

type ClockTable = [ClockTableBlock]

type ClockTableBlock = Block Text ClockTableFile

data ClockTableFile = ClockTableFile
    { clockTableFile :: RootedPath
    , clockTableForest :: Forest ClockTableHeaderEntry
    } deriving (Show, Eq, Generic)

instance Validity ClockTableFile

instance ToJSON ClockTableFile where
    toJSON ClockTableFile {..} =
        object ["file" .= clockTableFile, "forest" .= clockTableForest]

data ClockTableHeaderEntry = ClockTableHeaderEntry
    { clockTableHeaderEntryHeader :: Header
    , clockTableHeaderEntryTime :: NominalDiffTime
    } deriving (Show, Eq, Generic)

instance Validity ClockTableHeaderEntry

instance ToJSON ClockTableHeaderEntry where
    toJSON ClockTableHeaderEntry {..} =
        object
            [ "header" .= clockTableHeaderEntryHeader
            , "time" .= clockTableHeaderEntryTime
            ]

-- Intermediary types
type ClockTimeBlock a = Block a FileTimes

data FileTimes = FileTimes
    { clockTimeFile :: RootedPath
    , clockTimeForest :: TForest HeaderTimes
    } deriving (Generic)

data HeaderTimes f = HeaderTimes
    { headerTimesHeader :: Header
    , headerTimesEntries :: f LogbookEntry
    } deriving (Generic)

type TForest a = NonEmpty (TTree a)

data TTree a
    = TLeaf (a NonEmpty)
    | TBranch (a [])
              (TForest a)
    deriving (Generic)
