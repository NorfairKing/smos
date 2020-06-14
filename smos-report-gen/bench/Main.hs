{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main as Criterion
import Cursor.Simple.Forest
import Data.GenValidity.Containers ()
import Data.GenValidity.Criterion
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Smos.Data
import Smos.Data.Gen ()
import Smos.Report.Agenda
import Smos.Report.Agenda.Gen ()
import Smos.Report.Filter
import Smos.Report.Filter.Gen ()
import Smos.Report.Path
import Smos.Report.Time

main :: IO ()
main =
  Criterion.defaultMain
    [ bgroup
        "Filter"
        [ genValidBench @Part,
          genValidBench @Piece,
          genValidBench @Ast,
          genValidBench @(Filter RootedPath),
          genValidBench @(Filter Time),
          genValidBench @(Filter Tag),
          genValidBench @(Filter Header),
          genValidBench @(Filter TodoState),
          genValidBench @(Filter Timestamp),
          genValidBench @(Filter PropertyValue),
          genValidBench @(Filter Entry),
          genValidBench @(Filter (Maybe PropertyValue)),
          genValidBench @(Filter (Set Tag)),
          genValidBench @(Filter (Map PropertyName PropertyValue)),
          genValidBench @(Filter (ForestCursor Header)),
          genValidBench @(Filter (ForestCursor Entry)),
          genValidBench @(Filter (RootedPath, ForestCursor Entry))
        ],
      bgroup
        "Agenda"
        [ genValidBench @AgendaEntry,
          genValidBench @(AgendaTableBlock Text),
          genValidBench @AgendaTodayReport,
          genValidBench @AgendaReport
        ]
    ]
