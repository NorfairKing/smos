{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main as Criterion
import Data.GenValidity.Criterion
import Smos.Sync.Client.ContentsMap
import Smos.Sync.Client.ContentsMap.Gen
import Smos.Sync.Client.Env
import Smos.Sync.Client.MetaMap
import Smos.Sync.Client.MetaMap.Gen ()

main :: IO ()
main =
  Criterion.defaultMain
    [ genValidBench @SyncFileMeta,
      genValidBench @MetaMap,
      genValidBench @ContentsMap,
      genValidBench @ClientStore,
      genBench "sizedContentsMap 1000" (sizedContentsMap 1000)
    ]
