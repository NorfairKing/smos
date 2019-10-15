{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Sync.Client.DirForest.Gen where

import Data.ByteString
import Data.GenValidity
import qualified Data.Map as M
import Data.Maybe

import Path

import Test.QuickCheck

import Smos.API.Gen ()

import Smos.Sync.Client.Contents
import Smos.Sync.Client.DirForest
import Smos.Sync.Client.TestUtils

instance GenValid DirForest where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid DirOrFile where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
