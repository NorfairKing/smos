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
                0 -> oneof [pure ByFile, ByProperty <$> genUnchecked]
                _ ->
                    oneof
                        [ Reverse <$> scale pred genUnchecked
                        , do (a, b) <- genSplit n
                             s1 <- resize a genUnchecked
                             s2 <- resize b genUnchecked
                             pure $ AndThen s1 s2
                        ]

instance GenValid Sorter
