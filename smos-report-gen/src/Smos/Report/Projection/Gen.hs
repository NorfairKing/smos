{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.Projection.Gen where

import Data.GenValidity
import Data.GenValidity.Path ()
import Smos.Data.Gen ()
import Smos.Report.Path.Gen ()
import Smos.Report.Projection

instance GenValid Projection where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Projectee where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
