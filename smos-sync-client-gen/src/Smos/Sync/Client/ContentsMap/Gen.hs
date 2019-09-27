{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Sync.Client.ContentsMap.Gen where

import Debug.Trace

import Data.ByteString
import Data.GenValidity
import qualified Data.Map as M
import Data.Maybe

import Path

import Test.QuickCheck

import Smos.Sync.API.Gen ()

import Smos.Sync.Client.Contents
import Smos.Sync.Client.ContentsMap
import qualified Smos.Sync.Client.ContentsMap as CM
import Smos.Sync.Client.TestUtils

hideFile :: Path Rel File -> Path Rel File
hideFile f = fromJust $ parseRelFile $ '.' : toFilePath f

hideDir :: Path Rel Dir -> Path Rel Dir
hideDir d = fromJust $ parseRelDir $ '.' : toFilePath d

genHiddenFile :: Gen (Path Rel File)
genHiddenFile =
  oneof
    [ do d1 <- genValid
         d2 <- genValid
         f <- genValid
         pure $ d1 </> hideDir d2 </> f
    , do d <- genValid
         f <- genValid
         pure $ d </> hideFile f
    ]

filterHiddenFiles :: ContentsMap -> ContentsMap
filterHiddenFiles = ContentsMap . M.filterWithKey (\p _ -> isHidden p) . contentsMapFiles

instance GenValid ContentsMap where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

mapWithNewPath :: ContentsMap -> ByteString -> Gen (Path Rel File, ContentsMap)
mapWithNewPath = mapWithNewByGen genValid

mapWithNewHiddenPath :: ContentsMap -> ByteString -> Gen (Path Rel File, ContentsMap)
mapWithNewHiddenPath = mapWithNewByGen genHiddenFile

mapWithNewByGen ::
     Gen (Path Rel File) -> ContentsMap -> ByteString -> Gen (Path Rel File, ContentsMap)
mapWithNewByGen gen cm bs = gen `suchThatMap` (\p -> (,) p <$> CM.insert p bs cm)

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

changedMapsWithUnionOf :: ContentsMap -> Gen (ContentsMap, ContentsMap)
changedMapsWithUnionOf cm =
  (do cm1 <- genValid
      cm2 <- changedContentsMap cm1
      pure (cm1, cm2)) `suchThatMap`
  (\(cm1, cm2) -> (,) <$> CM.union cm1 cm <*> CM.union cm2 cm)

mapWithAdditions :: ContentsMap -> Gen ContentsMap
mapWithAdditions cm = genValid `suchThatMap` (\cm' -> CM.union cm' cm)

twoDistinctPathsThatFitAndTheirUnion ::
     ByteString -> ByteString -> Gen (Path Rel File, Path Rel File, ContentsMap)
twoDistinctPathsThatFitAndTheirUnion contents1 contents2 = do
  (rp1, rp2, Hidden func) <- twoDistinctPathsThatFitAndTheirUnionFunc
  pure (rp1, rp2, func contents1 contents2)

twoDistinctPathsThatFitAndTheirUnionFunc ::
     Gen (Path Rel File, Path Rel File, Hidden (ByteString -> ByteString -> ContentsMap))
twoDistinctPathsThatFitAndTheirUnionFunc = twoDistinctPathsThatFitAndTheirUnionWithFunc CM.empty

twoDistinctPathsThatFitAndTheirUnionWith ::
     ContentsMap -> ByteString -> ByteString -> Gen (Path Rel File, Path Rel File, ContentsMap)
twoDistinctPathsThatFitAndTheirUnionWith m contents1 contents2 = do
  (rp1, rp2, Hidden func) <- twoDistinctPathsThatFitAndTheirUnionWithFunc m
  pure (rp1, rp2, func contents1 contents2)

twoDistinctPathsThatFitAndTheirUnionWithFunc ::
     ContentsMap
  -> Gen (Path Rel File, Path Rel File, Hidden (ByteString -> ByteString -> ContentsMap))
twoDistinctPathsThatFitAndTheirUnionWithFunc cm = do
  (rp1, rp2, Hidden func) <- twoDistinctPathsThatFitAndTheirUnionsWithFunc cm
  let func' contents1 contents2 =
        let (_, _, m) = func contents1 contents2
         in m
  pure (rp1, rp2, Hidden func')

twoDistinctPathsThatFitAndTheirUnionsWith ::
     ContentsMap
  -> ByteString
  -> ByteString
  -> Gen (Path Rel File, Path Rel File, (ContentsMap, ContentsMap, ContentsMap))
twoDistinctPathsThatFitAndTheirUnionsWith cm contents1 contents2 = do
  (rp1, rp2, Hidden func) <- twoDistinctPathsThatFitAndTheirUnionsWithFunc cm
  pure (rp1, rp2, func contents1 contents2)

twoDistinctPathsThatFitAndTheirUnionsWithFunc ::
     ContentsMap
  -> Gen ( Path Rel File
         , Path Rel File
         , Hidden (ByteString -> ByteString -> (ContentsMap, ContentsMap, ContentsMap)))
twoDistinctPathsThatFitAndTheirUnionsWithFunc cm@(ContentsMap m) = do
  rp1 <- genValid `suchThat` (\rp -> isJust $ CM.insert rp "" cm)
  rp2 <-
    genValid `suchThat` (/= rp1) `suchThat`
    (\rp -> isJust $ CM.insert rp1 "" cm >>= CM.insert rp "")
  pure
    ( rp1
    , rp2
    , Hidden $ \contents1 contents2 ->
        ( ContentsMap $ M.insert rp1 contents1 m
        , ContentsMap $ M.insert rp2 contents2 m
        , ContentsMap $ M.unions [M.singleton rp1 contents1, M.singleton rp2 contents2, m]))

disjunctContentsMap :: ContentsMap -> Gen ContentsMap
disjunctContentsMap (ContentsMap m) = (ContentsMap <$> disjunctMap m) `suchThat` isValid

mapWithDisjunctUnion :: ContentsMap -> Gen (ContentsMap, ContentsMap)
mapWithDisjunctUnion cm = disjunctContentsMap cm `suchThatMap` (\cm' -> (,) cm' <$> CM.union cm' cm)

twoChangedMapsAndTheirUnions ::
     Gen ( ( ContentsMap
             -- m1
           , ContentsMap
             -- m2
            )
         , ( ContentsMap
             -- m1' = m3
           , ContentsMap
             -- m2' = m4
            )
         , ( ContentsMap
             -- m12
           , ContentsMap
             -- m14
           , ContentsMap
             -- m23
           , ContentsMap
             -- m34
            ))
twoChangedMapsAndTheirUnions = twoChangedMapsAndTheirUnionsWith CM.empty

twoChangedMapsAndTheirUnionsWith ::
     ContentsMap
  -> Gen ( ( ContentsMap
             -- m1
           , ContentsMap
             -- m2
            )
         , ( ContentsMap
             -- m1' = m3
           , ContentsMap
             -- m2' = m4
            )
         , ( ContentsMap
             -- m12
           , ContentsMap
             -- m14
           , ContentsMap
             -- m23
           , ContentsMap
             -- m34
            ))
twoChangedMapsAndTheirUnionsWith cm@(ContentsMap m) = do
  cm1@(ContentsMap m1) <- genValid `suchThat` (\cm1 -> isJust $ CM.union cm1 cm)
  cm2@(ContentsMap m2) <- genValid `suchThat` (\cm2 -> isJust $ CM.unions [cm2, cm1, cm])
  let cm12 = ContentsMap $ M.unions [m1, m2, m]
  cm3@(ContentsMap m3) <-
    changedContentsMap cm1 `suchThat` (\cm3 -> isJust $ CM.unions [cm3, cm2, cm1, cm])
  cm4@(ContentsMap m4) <-
    changedContentsMap cm2 `suchThat` (\cm4 -> isJust $ CM.unions [cm4, cm3, cm2, cm1, cm])
  let cm14 = ContentsMap $ M.unions [m1, m4, m]
  let cm23 = ContentsMap $ M.unions [m2, m3, m]
  let cm34 = ContentsMap $ M.unions [m3, m4, m]
  pure ((cm1, cm2), (cm3, cm4), (cm12, cm14, cm23, cm34))

threeDisjunctMapsAndTheirUnions ::
     Gen ( ( ContentsMap
          -- m1
           , ContentsMap
          -- m2
           , ContentsMap)
          -- m3)
         , ( ContentsMap
          -- m1 U m2
           , ContentsMap
          -- m2 U m3
           , ContentsMap
          -- m1 U m3
           , ContentsMap
          -- m1 U m2 U m3
            ))
threeDisjunctMapsAndTheirUnions = do
  cm1@(ContentsMap m1) <- genValid
  cm2@(ContentsMap m2) <- disjunctContentsMap cm1 `suchThat` (\cm2 -> isJust $ CM.union cm2 cm1)
  let cm12 = ContentsMap $ M.union m1 m2
  cm3@(ContentsMap m3) <-
    disjunctContentsMap cm12 `suchThat` (\cm3 -> isJust $ CM.unions [cm3, cm2, cm1])
  let cm23 = ContentsMap $ M.union m2 m3
  let cm13 = ContentsMap $ M.union m1 m3
  let cm123 = ContentsMap $ M.unions [m1, m2, m3]
  pure ((cm1, cm2, cm3), (cm12, cm23, cm13, cm123))

newtype Hidden a =
  Hidden a
  deriving (Eq, Ord)

instance Show (Hidden a) where
  show _ = "hidden"
