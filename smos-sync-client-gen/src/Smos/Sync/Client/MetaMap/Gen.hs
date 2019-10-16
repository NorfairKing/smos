{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Sync.Client.MetaMap.Gen where

import Data.ByteString
import Data.GenValidity
import qualified Data.Map as M
import Data.Maybe

import Path

import Test.QuickCheck

import Smos.API.Gen ()

import Smos.Sync.Client.MetaMap
import Smos.Sync.Client.TestUtils

instance GenValid MetaMap where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
