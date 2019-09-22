{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Sync.Client.ContentsMap.Gen where

import Data.ByteString
import Data.GenValidity
import qualified Data.Map as M
import Data.Maybe

import Path

import Test.QuickCheck

import Smos.Sync.API.Gen ()

import Smos.Sync.Client.ContentsMap
import qualified Smos.Sync.Client.ContentsMap as CM
import Smos.Sync.Client.TestUtils

instance GenValid ContentsMap where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

mapWithNewPath :: ContentsMap -> ByteString -> Gen (Path Rel File, ContentsMap)
mapWithNewPath cm bs = genValid `suchThatMap` (\p -> (,) p <$> CM.insert p bs cm)

mapsWithDifferentContentsAtNewPath :: ContentsMap -> Gen (ContentsMap, ContentsMap)
mapsWithDifferentContentsAtNewPath cm = do
  contents1 <- genValid
  contents2 <- genValid `suchThat` (/= contents1)
  genValid `suchThatMap` (\p -> (,) <$> CM.insert p contents1 cm <*> CM.insert p contents2 cm)

mapsWithDifferentContentsAtNewPath3 :: ContentsMap -> Gen (ContentsMap, ContentsMap, ContentsMap)
mapsWithDifferentContentsAtNewPath3 cm = do
  contents1 <- genValid
  contents2 <- genValid `suchThat` (/= contents1)
  contents3 <- genValid `suchThat` (/= contents1) `suchThat` (/= contents2)
  genValid `suchThatMap`
    (\p ->
       (,,) <$> CM.insert p contents1 cm <*> CM.insert p contents2 cm <*> CM.insert p contents3 cm)

changedContentsMap :: ContentsMap -> Gen ContentsMap
changedContentsMap (ContentsMap m) = ContentsMap <$> changedMap m

-- TODO this will not satisfy the constraints
changedMapsWithUnionOf :: ContentsMap -> Gen (ContentsMap, ContentsMap)
changedMapsWithUnionOf (ContentsMap m) = do
  m1c@(ContentsMap m1) <- genValid
  ContentsMap m2 <- changedContentsMap m1c
  pure (ContentsMap $ M.union m1 m, ContentsMap $ M.union m2 m)

-- TODO this will not satisfy the constraints
mapWithAdditions :: ContentsMap -> Gen ContentsMap
mapWithAdditions (ContentsMap m) = do
  ContentsMap m' <- genValid
  pure $ ContentsMap $ M.union m m'

-- TODO this will not satisfy the constraints
twoDistinctPathsThatFitAndTheirUnion ::
     ByteString -> ByteString -> Gen (Path Rel File, Path Rel File, ContentsMap)
twoDistinctPathsThatFitAndTheirUnion contents1 contents2 = do
  (rp1, rp2, func) <- twoDistinctPathsThatFitAndTheirUnionFunc
  pure (rp1, rp2, func contents1 contents2)

-- TODO this will not satisfy the constraints
twoDistinctPathsThatFitAndTheirUnionFunc ::
     Gen (Path Rel File, Path Rel File, (ByteString -> ByteString -> ContentsMap))
twoDistinctPathsThatFitAndTheirUnionFunc = twoDistinctPathsThatFitAndTheirUnionWithFunc CM.empty

-- TODO this will not satisfy the constraints
twoDistinctPathsThatFitAndTheirUnionWith ::
     ContentsMap -> ByteString -> ByteString -> Gen (Path Rel File, Path Rel File, ContentsMap)
twoDistinctPathsThatFitAndTheirUnionWith m contents1 contents2 = do
  (rp1, rp2, func) <- twoDistinctPathsThatFitAndTheirUnionWithFunc m
  pure (rp1, rp2, func contents1 contents2)

-- TODO this will not satisfy the constraints
twoDistinctPathsThatFitAndTheirUnionWithFunc ::
     ContentsMap -> Gen (Path Rel File, Path Rel File, (ByteString -> ByteString -> ContentsMap))
twoDistinctPathsThatFitAndTheirUnionWithFunc (ContentsMap m) = do
  rp1 <- genValid
  rp2 <- (genValid `suchThat` (/= rp1))
  pure
    ( rp1
    , rp2
    , \contents1 contents2 ->
        ContentsMap $ M.unions [(M.singleton rp1 contents1), (M.singleton rp2 contents2), m])

-- TODO this will not satisfy the constraints
disjunctContentsMap :: ContentsMap -> Gen ContentsMap
disjunctContentsMap (ContentsMap m) = ContentsMap <$> disjunctMap m

-- TODO this will not satisfy the constraints
mapWithDisjunctUnion :: ContentsMap -> Gen (ContentsMap, ContentsMap)
mapWithDisjunctUnion cm@(ContentsMap m) = do
  cm'@(ContentsMap m') <- disjunctContentsMap cm
  pure (cm', ContentsMap $ M.union m m')
