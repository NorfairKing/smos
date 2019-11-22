{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.Comparison.Gen where

import Data.Map (Map)
import Data.Set (Set)

import Data.GenValidity
import Data.GenValidity.Path ()

import Test.QuickCheck

import Cursor.Simple.Forest

import Smos.Data
import Smos.Data.Gen ()

import Smos.Report.Filter

import Smos.Report.Comparison
import Smos.Report.Path
import Smos.Report.Time
import Smos.Report.Time.Gen ()

instance GenUnchecked Comparison

instance GenValid Comparison
