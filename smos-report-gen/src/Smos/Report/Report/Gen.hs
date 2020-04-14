{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.Report.Gen where

import Data.GenValidity
import Data.GenValidity.Path ()
import Smos.Data.Gen ()
import Smos.Report.Archive.Gen ()
import Smos.Report.Filter.Gen ()
import Smos.Report.Path.Gen ()
import Smos.Report.Projection.Gen ()
import Smos.Report.Report
import Smos.Report.Sorter.Gen ()
import Smos.Report.Time.Gen ()

instance GenValid PreparedReport where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
