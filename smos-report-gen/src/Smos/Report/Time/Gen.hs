{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.Time.Gen where

import Data.GenValidity
import Smos.Data.Gen ()
import Smos.Report.Time

instance GenValid Time where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = \case
    Seconds i -> Seconds <$> shrinkValid i
    Minutes i -> Seconds (60 * i) : (Minutes <$> shrinkValid i)
    Hours i -> Minutes (60 * i) : (Hours <$> shrinkValid i)
    Days i -> Hours (24 * i) : (Days <$> shrinkValid i)
    Weeks i -> Days (7 * i) : (Weeks <$> shrinkValid i)
    Months i -> Days (30 * i) : (Days <$> shrinkValid i)
    Years i -> Months (12 * i) : (Years <$> shrinkValid i)
