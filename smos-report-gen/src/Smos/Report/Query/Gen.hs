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

instance GenUnchecked Filter where
    genUnchecked =
        sized $ \n ->
            case n of
                0 ->
                    oneof
                        [ FilterHasTag <$> genUnchecked
                        , FilterTodoState <$> genUnchecked
                        ]
                _ ->
                    oneof
                        [ FilterHasTag <$> genUnchecked
                        , FilterTodoState <$> genUnchecked
                        , FilterFile <$> genUnchecked
                        , FilterParent <$>
                          scale (max 0 . (\x -> x - 1)) genUnchecked
                        , FilterAncestor <$>
                          scale (max 0 . (\x -> x - 2)) genUnchecked
                        , FilterNot <$>
                          scale (max 0 . (\x -> x - 1)) genUnchecked
                        , do (a, b) <- genSplit =<< upTo n
                             FilterAnd <$> resize a genUnchecked <*>
                                 resize b genUnchecked
                        , do (a, b) <- genSplit =<< upTo n
                             FilterOr <$> resize a genUnchecked <*>
                                 resize b genUnchecked
                        ]

instance GenValid Filter where
    genValid = genValidStructurally

instance GenUnchecked PropertyFilter

instance GenValid PropertyFilter
