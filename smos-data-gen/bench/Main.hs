{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Typeable

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
    , genValidBench @Tag
    , genValidBench @TodoState
    , genValidBench @StateHistory
    , genValidBench @LogbookEntry
    , genValidBench @Logbook
    , genValidBench @Entry
    , genValidBench @SmosFile
    ]

genValidBench ::
     forall a. (Typeable a, GenValid a)
  => Benchmark
genValidBench = bench (show $ typeRep (Proxy @a)) $ whnfIO $ generate (genValid @a)
