{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Report.Path where

import Data.Aeson
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

instance ToJSON RootedPath where
    toJSON (Absolute p) = toJSON p
    toJSON (Relative ad rf) = object ["root" .= ad, "relative" .= rf]

resolveRootedPath :: RootedPath -> Path Abs File
resolveRootedPath (Relative ad rf) = ad </> rf
resolveRootedPath (Absolute af) = af
