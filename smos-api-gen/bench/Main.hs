{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main as Criterion
import Data.GenValidity.Criterion
import Smos.API
import Smos.API.Gen ()

main :: IO ()
main =
  Criterion.defaultMain
    [ genValidBench @Register,
      genValidBench @Login,
      genValidBench @ServerUUID,
      genValidBench @SyncFile,
      genValidBench @SyncRequest,
      genValidBench @SyncResponse
    ]
