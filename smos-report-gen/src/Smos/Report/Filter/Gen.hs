{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.Filter.Gen where

import Control.DeepSeq
import Cursor.Simple.Forest
import Data.GenValidity
import Data.GenValidity.Path ()
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Text as T
import Debug.Trace
import Path
import Smos.Data
import Smos.Data.Gen ()
import Smos.Report.Comparison.Gen ()
import Smos.Report.Filter
import Smos.Report.Time
import Smos.Report.Time.Gen ()
import Test.QuickCheck

instance GenValid Piece where
  genValid =
    Piece . T.pack
      <$> ( genListOf (genValid `suchThat` (validationIsValid . validateRestrictedChar))
              `suchThat` (not . null)
          )
  shrinkValid = shrinkValidStructurally

instance GenValid BinOp

instance GenValid Paren

instance GenValid Part where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Parts where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Ast where
  shrinkValid = shrinkValidStructurally
  genValid =
    sized $ \n ->
      case n of
        0 -> AstPiece <$> genValid
        _ ->
          oneof
            [ do
                (a, b) <- genSplit n
                AstUnOp <$> resize a genValid <*> resize b genValid,
              do
                (a, b, c) <- genSplit3 n
                AstBinOp <$> resize a genValid <*> resize b genValid <*> resize c genValid
            ]

instance GenValid (Filter (Path Rel File)) where
  genValid =
    withTopLevelBranches $
      ( FilterFile
          <$> ( genListOf (genValid `suchThat` (validationIsValid . validateRestrictedChar))
                  `suchThat` (not . null)
              )
          `suchThatMap` parseRelFile
      )
        `suchThat` isValid
  shrinkValid = shrinkValidFilter

instance GenValid (Filter Time) where
  genValid = withTopLevelBranches eqAndOrd
  shrinkValid = shrinkValidFilter

instance GenValid (Filter Tag) where
  genValid = withTopLevelBranches sub
  shrinkValid = shrinkValidFilter

instance GenValid (Filter Header) where
  genValid = withTopLevelBranches sub
  shrinkValid = shrinkValidFilter

instance GenValid (Filter TodoState) where
  genValid = withTopLevelBranches sub
  shrinkValid = shrinkValidFilter

instance GenValid (Filter Timestamp) where
  genValid = withTopLevelBranches eqAndOrd
  shrinkValid = shrinkValidFilter

instance GenValid (Filter PropertyValue) where
  genValid = withTopLevelBranches sub
  shrinkValid = shrinkValidFilter

instance GenValid (Filter Entry) where
  genValid =
    withTopLevelBranches $
      oneof
        [ FilterEntryHeader <$> genValid,
          FilterEntryTodoState <$> genValid,
          FilterEntryProperties <$> genValid,
          FilterEntryTags <$> genValid
        ]
  shrinkValid = shrinkValidFilter

instance GenValid (Filter a) => GenValid (Filter (Set a)) where
  genValid =
    withTopLevelBranches $
      oneof
        [ FilterAny <$> genValid,
          FilterAll <$> genValid
        ]
  shrinkValid = shrinkValidFilter

instance (GenValid (Filter a), GenValid (Filter b)) => GenValid (Filter (a, b)) where
  genValid =
    withTopLevelBranches $
      oneof
        [ FilterFst <$> genValid,
          FilterSnd <$> genValid
        ]
  shrinkValid = shrinkValidFilter

instance
  (Show k, Ord k, NFData k, GenValid k, FilterArgument k, GenValid (Filter v)) =>
  GenValid (Filter (Map k v))
  where
  genValid =
    withTopLevelBranches $
      oneof
        [ (FilterMapHas <$> genValid) `suchThat` isValid,
          (FilterMapVal <$> genValid <*> genValid) `suchThat` isValid
        ]
  shrinkValid = shrinkValidFilter

instance GenValid (Filter a) => GenValid (Filter (Maybe a)) where
  genValid = withTopLevelBranches $ FilterMaybe <$> genValid <*> genValid
  shrinkValid = shrinkValidFilter

instance GenValid (Filter a) => GenValid (Filter (ForestCursor a)) where
  genValid =
    withTopLevelBranches $
      sized $ \n ->
        let withoutRecursion =
              oneof
                [ FilterWithinCursor <$> genValid,
                  FilterLevel <$> genValid
                ]
         in case n of
              0 -> withoutRecursion
              _ ->
                oneof
                  [ withoutRecursion,
                    scale pred $ FilterParent <$> genValid,
                    scale pred $ FilterAncestor <$> genValid,
                    scale pred $ FilterChild <$> genValid,
                    scale pred $ FilterLegacy <$> genValid
                  ]
  shrinkValid = shrinkValidFilter

withEqAndOrToo ::
  (Show a, Ord a, NFData a, GenValid a, FilterArgument a, FilterOrd a) =>
  Gen (Filter a) ->
  Gen (Filter a)
withEqAndOrToo gen = frequency [(4, gen), (1, eqAndOrd)]

subEqOrd ::
  (Show a, Ord a, NFData a, GenValid a, FilterArgument a, FilterSubString a, FilterOrd a) =>
  Gen (Filter a)
subEqOrd = oneof [eqAndOrd, sub]

eqAndOrd :: (Show a, Ord a, NFData a, GenValid a, FilterArgument a, FilterOrd a) => Gen (Filter a)
eqAndOrd =
  sized $ \size -> do
    (a, b) <- genSplit size
    (FilterOrd <$> resize a genValid <*> resize b genValid) `suchThat` isValid

sub :: (Show a, Ord a, NFData a, GenValid a, FilterArgument a, FilterSubString a) => Gen (Filter a)
sub = (FilterSub <$> genValid) `suchThat` isValid

withTopLevelBranches :: Gen (Filter a) -> Gen (Filter a)
withTopLevelBranches gen =
  sized $ \n -> do
    let genNot = FilterNot <$> scale pred (withTopLevelBranches gen)
        genBin f = scale pred $
          sized $ \size -> do
            (a, b) <- genSplit size
            f <$> resize a (withTopLevelBranches gen) <*> resize b (withTopLevelBranches gen)
    traceShowM n
    case n of
      0 -> gen
      _ -> oneof [gen, genNot, genBin FilterAnd, genBin FilterOr]

shrinkValidFilter :: Filter a -> [Filter a]
shrinkValidFilter = go
  where
    goA :: FilterArgument a => a -> [a]
    goA a = [a' | Right a' <- parseArgument <$> shrinkValid (renderArgument a)]
    go :: Filter a -> [Filter a]
    go f =
      filter isValid $
        case f of
          FilterFile rf -> FilterFile <$> shrinkValid rf
          FilterPropertyTime f' -> FilterPropertyTime <$> go f'
          FilterEntryHeader f' -> FilterEntryHeader <$> go f'
          FilterEntryTodoState f' -> FilterEntryTodoState <$> go f'
          FilterEntryProperties f' -> FilterEntryProperties <$> go f'
          FilterEntryTags f' -> FilterEntryTags <$> go f'
          FilterWithinCursor f' -> FilterWithinCursor <$> go f'
          FilterLevel w -> FilterLevel <$> shrinkValid w
          FilterParent f' -> f' : (FilterParent <$> go f')
          FilterAncestor f' -> f' : FilterParent f' : (FilterAncestor <$> go f')
          FilterChild f' -> f' : (FilterChild <$> go f')
          FilterLegacy f' -> f' : FilterChild f' : (FilterLegacy <$> go f')
          FilterAny f' -> FilterAny <$> go f'
          FilterAll f' -> FilterAny f' : (FilterAll <$> go f')
          FilterMapHas a -> FilterMapHas <$> goA a
          FilterMapVal v f' -> FilterMapHas v : (FilterMapVal v <$> go f')
          FilterFst f' -> FilterFst <$> go f'
          FilterSnd f' -> FilterSnd <$> go f'
          FilterMaybe b f' -> FilterMaybe <$> shrinkValid b <*> go f'
          FilterSub a -> FilterSub <$> goA a
          FilterOrd o a -> FilterOrd <$> shrinkValid o <*> goA a
          FilterNot f' -> f' : (FilterNot <$> go f')
          FilterAnd f1 f2 -> f1 : f2 : [FilterAnd f1' f2' | (f1', f2') <- shrinkT2 go (f1, f2)]
          FilterOr f1 f2 -> f1 : f2 : [FilterOr f1' f2' | (f1', f2') <- shrinkT2 go (f1, f2)]
