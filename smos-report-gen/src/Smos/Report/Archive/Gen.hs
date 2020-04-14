{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.Archive.Gen where

import Data.GenValidity
import Smos.Report.Archive

instance GenValid HideArchive where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
