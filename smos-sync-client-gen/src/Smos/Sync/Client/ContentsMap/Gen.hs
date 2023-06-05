{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Sync.Client.ContentsMap.Gen where

import Data.ByteString (ByteString)
import qualified Data.DirForest as DF
import Data.GenValidity
import Data.Maybe
import Path
import Smos.API.Gen ()
import Smos.Sync.Client.ContentsMap
import qualified Smos.Sync.Client.ContentsMap as CM
import Smos.Sync.Client.TestUtils
import Test.QuickCheck

hideFile :: Path Rel File -> Path Rel File
hideFile f = fromJust $ parseRelFile $ '.' : toFilePath f

hideDir :: Path Rel Dir -> Path Rel Dir
hideDir d = fromJust $ parseRelDir $ '.' : toFilePath d

genHiddenFile :: Gen (Path Rel File)
genHiddenFile =
  oneof
    [ do
        d1 <- genValid
        d2 <- genValid
        f <- genValid
        pure $ d1 </> hideDir d2 </> f,
      do
        d <- genValid
        f <- genValid
        pure $ d </> hideFile f
    ]

instance GenValid ContentsMap where
  shrinkValid = shrinkValidStructurally
  genValid = genValidStructurally

sizedContentsMap :: Int -> Gen ContentsMap
sizedContentsMap 0 = pure CM.empty
sizedContentsMap i = scale (`div` i) $ go i
  where
    go 0 = pure CM.empty
    go j = do
      contentsMap <- go (j - 1)
      let ins cm = do
            path <- genValid
            contents <- genValid
            case CM.insert path contents cm of
              Nothing -> ins cm
              Just cm' -> pure cm'
      ins contentsMap

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
  genValid
    `suchThatMap` ( \p ->
                      (,,) <$> CM.insert p contents1 cm <*> CM.insert p contents2 cm <*> CM.insert p contents3 cm
                  )

changedContentsMap :: ContentsMap -> Gen ContentsMap
changedContentsMap (ContentsMap m) = ContentsMap <$> changedDirForest m

changedMapsWithUnionOf :: ContentsMap -> Gen (ContentsMap, ContentsMap)
changedMapsWithUnionOf cm =
  ( do
      cm1 <- genValid
      cm2 <- changedContentsMap cm1
      pure (cm1, cm2)
  )
    `suchThatMap` (\(cm1, cm2) -> (,) <$> CM.union cm1 cm <*> CM.union cm2 cm)

mapWithAdditions :: ContentsMap -> Gen ContentsMap
mapWithAdditions cm = genValid `suchThatMap` (`CM.union` cm)

mapWithHiddenAdditions :: ContentsMap -> Gen ContentsMap
mapWithHiddenAdditions cm =
  let dfg =
        (DF.fromFileList <$> genListOf1 ((,) <$> genHiddenFile <*> genValid))
          `suchThatMap` ( \case
                            Left _ -> Nothing
                            Right r -> Just r
                        )
   in (ContentsMap <$> dfg)
        `suchThatMap` (`CM.union` cm)

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
  ContentsMap ->
  Gen (Path Rel File, Path Rel File, Hidden (ByteString -> ByteString -> ContentsMap))
twoDistinctPathsThatFitAndTheirUnionWithFunc cm = do
  (rp1, rp2, Hidden func) <- twoDistinctPathsThatFitAndTheirUnionsWithFunc cm
  let func' contents1 contents2 =
        let (_, _, m) = func contents1 contents2
         in m
  pure (rp1, rp2, Hidden func')

twoDistinctPathsThatFitAndTheirUnionsWith ::
  ContentsMap ->
  ByteString ->
  ByteString ->
  Gen (Path Rel File, Path Rel File, (ContentsMap, ContentsMap, ContentsMap))
twoDistinctPathsThatFitAndTheirUnionsWith cm contents1 contents2 = do
  (rp1, rp2, Hidden func) <- twoDistinctPathsThatFitAndTheirUnionsWithFunc cm
  pure (rp1, rp2, func contents1 contents2)

twoDistinctPathsThatFitAndTheirUnionsWithFunc ::
  ContentsMap ->
  Gen
    ( Path Rel File,
      Path Rel File,
      Hidden (ByteString -> ByteString -> (ContentsMap, ContentsMap, ContentsMap))
    )
twoDistinctPathsThatFitAndTheirUnionsWithFunc cm = do
  rp1 <- genValid `suchThat` (\rp -> isJust $ CM.insert rp "" cm)
  rp2 <-
    genValid
      `suchThat` (/= rp1)
      `suchThat` (\rp -> isJust $ CM.insert rp1 "" cm >>= CM.insert rp "")
  pure
    ( rp1,
      rp2,
      Hidden $ \contents1 contents2 ->
        ( fromJust $ CM.insert rp1 contents1 cm, -- Safe because we just checked it above
          fromJust $ CM.insert rp2 contents2 cm, -- Safe because we just checked it above
          fromJust $ CM.unions [CM.singleton rp1 contents1, CM.singleton rp2 contents2, cm] -- Safe because we just checked it above
        )
    )

disjunctContentsMap :: ContentsMap -> Gen ContentsMap
disjunctContentsMap (ContentsMap m) = (ContentsMap <$> disjunctDirForest m) `suchThat` isValid

mapWithDisjunctUnion :: ContentsMap -> Gen (ContentsMap, ContentsMap)
mapWithDisjunctUnion cm = disjunctContentsMap cm `suchThatMap` (\cm' -> (,) cm' <$> CM.union cm' cm)

twoChangedMapsAndTheirUnions ::
  Gen
    ( ( ContentsMap,
        -- m1
        ContentsMap
        -- m2
      ),
      ( ContentsMap,
        -- m1' = m3
        ContentsMap
        -- m2' = m4
      ),
      ( ContentsMap,
        -- m12
        ContentsMap,
        -- m14
        ContentsMap,
        -- m23
        ContentsMap
        -- m34
      )
    )
twoChangedMapsAndTheirUnions = twoChangedMapsAndTheirUnionsWith CM.empty

twoChangedMapsAndTheirUnionsWith ::
  ContentsMap ->
  Gen
    ( ( ContentsMap,
        -- m1
        ContentsMap
        -- m2
      ),
      ( ContentsMap,
        -- m1' = m3
        ContentsMap
        -- m2' = m4
      ),
      ( ContentsMap,
        -- m1 U m2
        ContentsMap,
        -- m1 U m4
        ContentsMap,
        -- m2 U m3
        ContentsMap
        -- m3 U m4
      )
    )
twoChangedMapsAndTheirUnionsWith cm = do
  (cm1, cm1m) <- genValid `suchThatMap` (\cm1 -> (,) cm1 <$> CM.union cm1 cm)
  (cm2, cm12m) <- genValid `suchThatMap` (\cm2 -> (,) cm2 <$> CM.union cm2 cm1m)
  (cm3, cm123m) <- changedContentsMap cm1 `suchThatMap` (\cm3 -> (,) cm3 <$> CM.union cm3 cm12m)
  (cm4, _) <- changedContentsMap cm2 `suchThatMap` (\cm4 -> (,) cm4 <$> CM.union cm4 cm123m)
  let cm12 = fromJust $ CM.unions [cm1, cm2, cm] -- Safe because we just checked above
  let cm23 = fromJust $ CM.unions [cm2, cm3, cm] -- Safe because we just checked above
  let cm34 = fromJust $ CM.unions [cm3, cm4, cm] -- Safe because we just checked above
  let cm14 = fromJust $ CM.unions [cm1, cm4, cm] -- Safe because we just checked above
  pure ((cm1, cm2), (cm3, cm4), (cm12, cm14, cm23, cm34))

threeDisjunctMapsAndTheirUnions ::
  Gen
    ( ( ContentsMap,
        -- m1
        ContentsMap,
        -- m2
        ContentsMap
      ),
      -- m3)
      ( ContentsMap,
        -- m1 U m2
        ContentsMap,
        -- m2 U m3
        ContentsMap,
        -- m1 U m3
        ContentsMap
        -- m1 U m2 U m3
      )
    )
threeDisjunctMapsAndTheirUnions = do
  cm1 <- genValid
  (cm2, cm12) <- disjunctContentsMap cm1 `suchThatMap` (\cm2 -> (,) cm2 <$> CM.union cm2 cm1)
  (cm3, cm123) <- disjunctContentsMap cm12 `suchThatMap` (\cm3 -> (,) cm3 <$> CM.union cm3 cm12)
  let cm23 = fromJust $ CM.union cm2 cm3 -- Safe because we just checked above
  let cm13 = fromJust $ CM.union cm1 cm3 -- Safe because we just checked above
  pure ((cm1, cm2, cm3), (cm12, cm23, cm13, cm123))

newtype Hidden a
  = Hidden a
  deriving (Eq, Ord)

instance Show (Hidden a) where
  show _ = "hidden"
