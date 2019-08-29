{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.Time.Gen where

import Data.GenValidity

import Smos.Data.Gen ()

import Smos.Report.Time

instance GenValid Time where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
