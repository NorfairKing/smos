{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.Projection.Gen where

import Data.GenValidity
import Data.GenValidity.Path ()

import Smos.Data.Gen ()

import Smos.Report.Path.Gen ()

import Smos.Report.Projection

instance GenUnchecked Projection

instance GenValid Projection

instance GenUnchecked Projectee

instance GenValid Projectee
