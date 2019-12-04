{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Map (Map)
import Data.Set (Set)
import Data.Typeable

import Data.GenValidity.Containers ()
import Data.GenValidity.Criterion
import Test.QuickCheck

import Criterion.Main as Criterion

import Smos.Data
import Smos.Data.Gen ()

main :: IO ()
main =
  Criterion.defaultMain
    [ genValidBench @Header
    , genValidBench @Contents
    , genValidBench @PropertyName
    , genValidBench @PropertyValue
    , genValidBench @TimestampName
    , genValidBench @Timestamp
    , genValidBench @TodoState
    , genValidBench @StateHistoryEntry
    , genValidBench @StateHistory
    , genValidBench @LogbookEntry
    , genValidBench @Logbook
    , genValidBench @Tag
    , genValidBench @(Set Tag)
    , genValidBench @(Map TimestampName Timestamp)
    , genValidBench @(Map PropertyName PropertyValue)
    , genValidBench @Entry
    , genValidBench @(Tree Entry)
    , genValidBench @(Forest Entry)
    , genValidBench @SmosFile
    ]
