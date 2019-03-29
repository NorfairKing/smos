{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.Sorter.Gen where

import Data.GenValidity
import Data.GenValidity.Path ()

import Test.QuickCheck

import Smos.Data.Gen ()

import Smos.Report.Sorter

instance GenUnchecked Sorter where
    genUnchecked =
        sized $ \n ->
            case n of
                0 -> oneof [pure ByFile]
                _ -> oneof [ByProperty <$> genUnchecked]

instance GenValid Sorter where
    genValid = genValidStructurally
