{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.GenValidity.Criterion

import Criterion.Main as Criterion

import Smos.Sync.Client.ContentsMap
import Smos.Sync.Client.ContentsMap.Gen ()
import Smos.Sync.Client.Env
import Smos.Sync.Client.MetaMap
import Smos.Sync.Client.MetaMap.Gen ()

main :: IO ()
main =
  Criterion.defaultMain
    [ genValidBench @SyncFileMeta
    , genValidBench @MetaMap
    , genValidBench @ContentsMap
    , genValidBench @ClientStore
    ]
