{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.Query.Gen where

import Data.GenValidity
import Data.GenValidity.Path ()

import Test.QuickCheck

import Smos.Data.Gen ()

import Smos.Report.Query

instance GenValid Filter where
  genValid =
    sized $ \n ->
      case n of
        0 -> oneof [FilterHasTag <$> genValid, FilterTodoState <$> genValid]
        _ ->
          oneof
            [ FilterHasTag <$> genValid
            , FilterTodoState <$> genValid
            , (FilterFile <$> genValid) `suchThat` isValid
            , FilterParent <$> scale (max 0 . (\x -> x - 1)) genValid
            , FilterAncestor <$> scale (max 0 . (\x -> x - 2)) genValid
            , FilterNot <$> scale (max 0 . (\x -> x - 1)) genValid
            , do (a, b) <- genSplit =<< upTo n
                 FilterAnd <$> resize a genValid <*> resize b genValid
            , do (a, b) <- genSplit =<< upTo n
                 FilterOr <$> resize a genValid <*> resize b genValid
            ]
  shrinkValid = shrinkValidStructurally

instance GenValid PropertyFilter where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
