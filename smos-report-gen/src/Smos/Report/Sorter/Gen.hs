{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.Sorter.Gen where

import Data.GenValidity
import Data.GenValidity.Path ()
import Smos.Data.Gen ()
import Smos.Report.Sorter
import Test.QuickCheck

instance GenValid Sorter where
  genValid =
    sized $ \n ->
      let withoutRecursion =
            oneof
              [ pure ByFile,
                pure ByState,
                ByTag <$> genValid,
                ByProperty <$> genValid,
                ByPropertyTime <$> genValid
              ]
       in case n of
            0 -> withoutRecursion
            _ ->
              oneof
                [ withoutRecursion,
                  Reverse <$> scale pred genValid,
                  do
                    (a, b) <- genSplit n
                    s1 <- resize a genValid
                    s2 <- resize b genValid
                    pure $ AndThen s1 s2
                ]
  shrinkValid = shrinkValidStructurally
