{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Smos.Report.Path where

import Data.Validity
import Data.Validity.Path ()
import GHC.Generics (Generic)

import Path


data RootedPath
    = Relative (Path Abs Dir)
               (Path Rel File)
    | Absolute (Path Abs File)
    deriving (Show, Eq, Generic)

instance Validity RootedPath
