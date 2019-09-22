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
  (rp1, rp2, Hidden func) <- twoDistinctPathsThatFitAndTheirUnionFunc
  pure (rp1, rp2, func contents1 contents2)

-- TODO this will not satisfy the constraints
twoDistinctPathsThatFitAndTheirUnionFunc ::
     Gen (Path Rel File, Path Rel File, Hidden (ByteString -> ByteString -> ContentsMap))
twoDistinctPathsThatFitAndTheirUnionFunc = twoDistinctPathsThatFitAndTheirUnionWithFunc CM.empty

-- TODO this will not satisfy the constraints
twoDistinctPathsThatFitAndTheirUnionWith ::
     ContentsMap -> ByteString -> ByteString -> Gen (Path Rel File, Path Rel File, ContentsMap)
twoDistinctPathsThatFitAndTheirUnionWith m contents1 contents2 = do
  (rp1, rp2, Hidden func) <- twoDistinctPathsThatFitAndTheirUnionWithFunc m
  pure (rp1, rp2, func contents1 contents2)

-- TODO this will not satisfy the constraints
twoDistinctPathsThatFitAndTheirUnionWithFunc ::
     ContentsMap
  -> Gen (Path Rel File, Path Rel File, Hidden (ByteString -> ByteString -> ContentsMap))
twoDistinctPathsThatFitAndTheirUnionWithFunc cm = do
  (rp1, rp2, Hidden func) <- twoDistinctPathsThatFitAndTheirUnionsWithFunc cm
  let func' contents1 contents2 =
        let (_, _, m) = func contents1 contents2
         in m
  pure (rp1, rp2, Hidden func')

-- TODO this will not satisfy the constraints
twoDistinctPathsThatFitAndTheirUnionsWith ::
     ContentsMap
  -> ByteString
  -> ByteString
  -> Gen (Path Rel File, Path Rel File, (ContentsMap, ContentsMap, ContentsMap))
twoDistinctPathsThatFitAndTheirUnionsWith cm contents1 contents2 = do
  (rp1, rp2, Hidden func) <- twoDistinctPathsThatFitAndTheirUnionsWithFunc cm
  pure (rp1, rp2, func contents1 contents2)

-- TODO this will not satisfy the constraints
twoDistinctPathsThatFitAndTheirUnionsWithFunc ::
     ContentsMap
  -> Gen ( Path Rel File
         , Path Rel File
         , (Hidden (ByteString -> ByteString -> (ContentsMap, ContentsMap, ContentsMap))))
twoDistinctPathsThatFitAndTheirUnionsWithFunc (ContentsMap m) = do
  rp1 <- genValid
  rp2 <- (genValid `suchThat` (/= rp1))
  pure
    ( rp1
    , rp2
    , Hidden $ \contents1 contents2 ->
        ( ContentsMap $ M.insert rp1 contents1 m
        , ContentsMap $ M.insert rp2 contents2 m
        , ContentsMap $ M.unions [(M.singleton rp1 contents1), (M.singleton rp2 contents2), m]))

-- TODO this will not satisfy the constraints
disjunctContentsMap :: ContentsMap -> Gen ContentsMap
disjunctContentsMap (ContentsMap m) = ContentsMap <$> disjunctMap m

-- TODO this will not satisfy the constraints
mapWithDisjunctUnion :: ContentsMap -> Gen (ContentsMap, ContentsMap)
mapWithDisjunctUnion cm@(ContentsMap m) = do
  cm'@(ContentsMap m') <- disjunctContentsMap cm
  pure (cm', ContentsMap $ M.union m m')

-- TODO this will not satisfy the constraints
twoChangedMapsAndTheirUnions ::
     Gen ( ( ContentsMap
            -- ^ m1
           , ContentsMap
            -- ^ m2
            )
         , ( ContentsMap
            -- ^ m1' = m3
           , ContentsMap
            -- ^ m2' = m4
            )
         , ( ContentsMap
            -- ^ m12
           , ContentsMap
            -- ^ m14
           , ContentsMap
            -- ^ m23
           , ContentsMap
            -- ^ m34
            ))
twoChangedMapsAndTheirUnions = twoChangedMapsAndTheirUnionsWith CM.empty

twoChangedMapsAndTheirUnionsWith ::
     ContentsMap
  -> Gen ( ( ContentsMap
            -- ^ m1
           , ContentsMap
            -- ^ m2
            )
         , ( ContentsMap
            -- ^ m1' = m3
           , ContentsMap
            -- ^ m2' = m4
            )
         , ( ContentsMap
            -- ^ m12
           , ContentsMap
            -- ^ m14
           , ContentsMap
            -- ^ m23
           , ContentsMap
            -- ^ m34
            ))
twoChangedMapsAndTheirUnionsWith (ContentsMap m) = do
  cm1@(ContentsMap m1) <- genValid
  cm2@(ContentsMap m2) <- genValid
  let cm12 = ContentsMap $ M.unions [m1, m2, m]
  cm3@(ContentsMap m3) <- changedContentsMap cm1
  cm4@(ContentsMap m4) <- changedContentsMap cm2
  let cm14 = ContentsMap $ M.unions [m1, m4, m]
  let cm23 = ContentsMap $ M.unions [m2, m3, m]
  let cm34 = ContentsMap $ M.unions [m3, m4, m]
  pure ((cm1, cm2), (cm3, cm4), (cm12, cm14, cm23, cm34))

-- TODO this will not satisfy the constraints
threeDisjunctMapsAndTheirUnions ::
     Gen ( ( ContentsMap
          -- ^ m1
           , ContentsMap
          -- ^ m2
           , ContentsMap)
          -- ^ m3)
         , ( ContentsMap
          -- ^ m1 U m2
           , ContentsMap
          -- ^ m2 U m3
           , ContentsMap
          -- ^ m1 U m3
           , ContentsMap
          -- ^ m1 U m2 U m3
            ))
threeDisjunctMapsAndTheirUnions = do
  cm1@(ContentsMap m1) <- genValid
  cm2@(ContentsMap m2) <- disjunctContentsMap cm1
  let cm12 = ContentsMap $ M.union m1 m2
  cm3@(ContentsMap m3) <- disjunctContentsMap cm12
  let cm23 = ContentsMap $ M.union m2 m3
  let cm13 = ContentsMap $ M.union m1 m3
  let cm123 = ContentsMap $ M.unions [m1, m2, m3]
  pure ((cm1, cm2, cm3), (cm12, cm23, cm13, cm123))

newtype Hidden a =
  Hidden a
  deriving (Eq, Ord)

instance Show (Hidden a) where
  show _ = "hidden"
