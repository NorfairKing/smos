{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main as Criterion
import Data.GenValidity.Containers ()
import Data.GenValidity.Criterion
import Data.Map (Map)
import Data.Set (Set)
import Path
import Path.IO
import Smos.Data
import Smos.Data.Gen ()

main :: IO ()
main = do
  here <- getCurrentDir
  Criterion.defaultMain
    [ bench "readSmosFile" $ nfIO $ readSmosFile $ here </> [relfile|test_resources/golden/single-complex-entry.smos|],
      bgroup
        "generators"
        [ genValidBench @Header,
          genValidBench @Contents,
          genValidBench @PropertyName,
          genValidBench @PropertyValue,
          genValidBench @TimestampName,
          genValidBench @Timestamp,
          genValidBench @TodoState,
          genValidBench @StateHistoryEntry,
          genValidBench @StateHistory,
          genValidBench @LogbookEntry,
          genValidBench @Logbook,
          genValidBench @Tag,
          genValidBench @(Set Tag),
          genValidBench @(Map TimestampName Timestamp),
          genValidBench @(Map PropertyName PropertyValue),
          genValidBench @Entry,
          genValidBench @(Tree Entry),
          genValidBench @(Forest Entry),
          genValidBench @SmosFile
        ]
    ]
