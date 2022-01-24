{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Server.Gen where

import Data.GenValidity
import Data.GenValidity.Persist ()
import Data.GenValidity.Time ()
import Data.GenValidity.UUID.Typed ()
import Smos.Server.DB

instance GenValid Backup
