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
      let withoutRecursion =
            oneof
              [ pure ByFile
              , ByTag <$> genValid
              , ByProperty <$> genValid
              , ByPropertyTime <$> genValid
              ]
       in case n of
            0 -> withoutRecursion
            _ ->
              oneof
                [ withoutRecursion
                , Reverse <$> scale pred genValid
                , do (a, b) <- genSplit n
                     s1 <- resize a genValid
                     s2 <- resize b genValid
                     pure $ AndThen s1 s2
                ]
  shrinkValid = shrinkValidStructurally
