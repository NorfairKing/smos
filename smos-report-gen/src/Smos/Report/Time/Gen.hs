{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.Time.Gen where

import Data.GenValidity
import Debug.Trace
import Smos.Data.Gen ()
import Smos.Report.Time

instance GenValid Time where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = \case
    Seconds i -> Seconds <$> shrinkValid i
    Minutes i -> Seconds (60 * i) : (Minutes <$> shrinkValid i)
    Hours i -> Minutes (60 * i) : (Hours <$> shrinkValid i)
    Days i -> Days (24 * i) : (Days <$> shrinkValid i)
    Weeks i -> Weeks (7 * i) : (Weeks <$> shrinkValid i)
