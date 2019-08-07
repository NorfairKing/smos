{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Smos.Report.Work where

import GHC.Generics (Generic)

import Data.Validity
import Data.Validity.Path ()

import Smos.Data

import Smos.Report.Path

data WorkReport =
  WorkReport
    { workReportEntries :: [(RootedPath, Entry)]
    }
  deriving (Show, Eq, Generic)

instance Validity WorkReport
