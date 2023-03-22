{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Directory.Archive.Gen where

import Data.GenValidity
import Smos.Directory.Archive

instance GenValid HideArchive where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
