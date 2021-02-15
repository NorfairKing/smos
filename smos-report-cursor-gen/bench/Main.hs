{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main as Criterion
import Cursor.Simple.Forest
import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.Criterion
import Data.GenValidity.Path ()
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time
import Path
import Smos.Cursor.Report.Entry
import Smos.Cursor.Report.Entry.Gen
import Smos.Cursor.Report.Stuck
import Smos.Cursor.Report.Stuck.Gen
import Smos.Cursor.Report.Timestamps
import Smos.Cursor.Report.Timestamps.Gen
import Smos.Cursor.Report.Waiting
import Smos.Cursor.Report.Waiting.Gen
import Smos.Cursor.Report.Work
import Smos.Cursor.Report.Work.Gen
import Smos.Data
import Smos.Data.Gen ()
import Smos.Report.Stuck
import Smos.Report.Stuck.Gen
import Smos.Report.Time

main :: IO ()
main =
  Criterion.defaultMain
    [ bgroup
        "Entry"
        [ genValidBench @(EntryReportCursor ()),
          genBench "genNonEmptyValidEntryReportCursorWith" $
            genNonEmptyValidEntryReportCursorWith
              (\_ _ -> [()])
              id
              genValid,
          genValidBench @(EntryReportEntryCursor ()),
          genValidBench @EntryReportCursorSelection
        ],
      bgroup
        "Waiting"
        [ genValidBench @WaitingReportCursor,
          genBench "genNonEmptyWaitingReportCursor" genNonEmptyWaitingReportCursor,
          genValidBench @(EntryReportCursor UTCTime)
        ],
      bgroup
        "Stuck"
        [ genValidBench @StuckReportCursor,
          genBench "genNonEmptyStuckReportCursor" genNonEmptyStuckReportCursor,
          genValidBench @StuckReportEntry
        ],
      bgroup
        "Timestamps"
        [ genValidBench @TimestampsReportCursor,
          genBench "genNonEmptyTimestampsReportCursor" genNonEmptyTimestampsReportCursor,
          genValidBench @TimestampsEntryCursor
        ],
      bgroup
        "Work"
        [ genValidBench @(EntryReportEntryCursor (TimestampName, Timestamp)),
          genValidBench @WorkReportCursor
        ]
    ]
