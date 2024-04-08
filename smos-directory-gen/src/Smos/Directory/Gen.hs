{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Directory.Gen where

import Data.GenValidity
import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import Smos.Directory.OptParse.Types

instance GenValid DirectoryConfiguration
