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

instance GenValid Sorter where
  genValid =
    sized $ \n ->
      case n of
        0 -> oneof [pure ByFile, ByProperty <$> genValid]
        _ ->
          oneof
            [ Reverse <$> scale pred genValid
            , do (a, b) <- genSplit n
                 s1 <- resize a genValid
                 s2 <- resize b genValid
                 pure $ AndThen s1 s2
            ]
  shrinkValid = shrinkValidStructurally
