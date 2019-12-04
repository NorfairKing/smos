{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Map (Map)
import Data.Set (Set)
import Data.Typeable

import Data.GenValidity.Containers()
import Data.GenValidity
import Test.QuickCheck

import Gauge.Main as Gauge

import Smos.Data
import Smos.Data.Gen ()

main :: IO ()
main =
  Gauge.defaultMain
    [ genValidBench @Header
    , genValidBench @Contents
    , genValidBench @PropertyName
    , genValidBench @PropertyValue
    , genValidBench @TimestampName
    , genValidBench @Timestamp
    , genValidBench @TodoState
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

genValidBench ::
     forall a. (Typeable a, GenValid a)
  => Benchmark
genValidBench = bench (show $ typeRep (Proxy @a)) $ whnfIO $ generate (resize 30 $ genValid @a)
