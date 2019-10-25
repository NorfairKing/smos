{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.Filter.Gen where

import Data.Map (Map)
import Data.Text (Text)

import Data.GenValidity
import Data.GenValidity.Path ()

import Test.QuickCheck

import Cursor.Simple.Forest

import Smos.Data
import Smos.Data.Gen ()

import Smos.Report.Filter

import Smos.Report.Path
import Smos.Report.Time
import Smos.Report.Time.Gen ()

instance GenValid (Filter RootedPath) where
  genValid = withTopLevelBranches $ FilterFile <$> genValid
  shrinkValid _ = []

instance GenValid (Filter Time) where
  genValid = withTopLevelBranches eqAndOrd
  shrinkValid _ = []

instance GenValid (Filter Tag) where
  genValid = withTopLevelBranches subEqOrd
  shrinkValid _ = []

instance GenValid (Filter Header) where
  genValid = withTopLevelBranches subEqOrd
  shrinkValid _ = []

instance GenValid (Filter TodoState) where
  genValid = withTopLevelBranches subEqOrd
  shrinkValid _ = []

instance GenValid (Filter Timestamp) where
  genValid = withTopLevelBranches eqAndOrd
  shrinkValid _ = []

instance GenValid (Filter PropertyValue) where
  genValid = withTopLevelBranches subEqOrd
  shrinkValid _ = []

instance GenValid (Filter Entry) where
  genValid =
    withTopLevelBranches $
    oneof
      [ FilterEntryHeader <$> genValid
      , FilterEntryTodoState <$> genValid
      , FilterEntryTimestamps <$> genValid
      , FilterEntryProperties <$> genValid
      , FilterEntryTags <$> genValid
      ]
  shrinkValid _ = []

-- TODO this doesn't allow for `FilterAll` non-FilterArgument a ...
instance (Show a, Ord a, GenValid a, FilterArgument a, GenValid (Filter a)) =>
         GenValid (Filter [a]) where
  genValid =
    withTopLevelBranches $
    sized $ \n ->
      let withoutRecursion = FilterListHas <$> genValid
       in case n of
            0 -> withoutRecursion
            _ -> oneof [withoutRecursion, FilterAny <$> genValid, FilterAll <$> genValid]
  shrinkValid _ = []

instance (GenValid (Filter a), GenValid (Filter b)) => GenValid (Filter (a, b)) where
  genValid = withTopLevelBranches $ oneof [FilterFst <$> genValid, FilterSnd <$> genValid]
  shrinkValid _ = []

instance (Show k, Ord k, GenValid k, FilterArgument k, GenValid (Filter v)) =>
         GenValid (Filter (Map k v)) where
  genValid =
    withTopLevelBranches $ oneof [FilterMapHas <$> genValid, FilterMapVal <$> genValid <*> genValid]
  shrinkValid _ = []

instance GenValid (Filter a) => GenValid (Filter (Maybe a)) where
  genValid = withTopLevelBranches $ FilterMaybe <$> genValid <*> genValid
  shrinkValid _ = []

instance GenValid (Filter a) => GenValid (Filter (ForestCursor a)) where
  genValid =
    withTopLevelBranches $
    sized $ \n ->
      let withoutRecursion = oneof [FilterWithinCursor <$> genValid, FilterLevel <$> genValid]
       in case n of
            0 -> withoutRecursion
            _ ->
              oneof
                [ withoutRecursion
                , FilterParent <$> genValid
                , FilterAncestor <$> genValid
                , FilterChild <$> genValid
                , FilterLegacy <$> genValid
                ]
  shrinkValid _ = []

withEqAndOrToo :: (Show a, Ord a, GenValid a, FilterArgument a) => Gen (Filter a) -> Gen (Filter a)
withEqAndOrToo gen = frequency [(4, gen), (1, eqAndOrd)]

subEqOrd :: (Show a, Ord a, FilterArgument a, FilterSubString a, GenValid a) => Gen (Filter a)
subEqOrd = oneof [eqAndOrd, sub]

eqAndOrd :: (Show a, Ord a, GenValid a, FilterArgument a) => Gen (Filter a)
eqAndOrd = FilterOrd <$> genValid <*> genValid

sub :: (Show a, Ord a, FilterArgument a, FilterSubString a, GenValid a) => Gen (Filter a)
sub = FilterSub <$> genValid

withTopLevelBranches :: Gen (Filter a) -> Gen (Filter a)
withTopLevelBranches gen =
  sized $ \n ->
    case n of
      0 -> gen
      _ ->
        let bin f = do
              (a, b) <- genSplit n
              f <$> resize a gen <*> resize b gen
         in oneof [FilterNot <$> gen, bin FilterAnd, bin FilterOr]
