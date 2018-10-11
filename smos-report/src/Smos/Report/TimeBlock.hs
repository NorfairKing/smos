{-# LANGUAGE DeriveGeneric #-}

module Smos.Report.TimeBlock where

import GHC.Generics (Generic)

import Data.Validity

data TimeBlock
    = OneBlock
    | DayBlock
    deriving (Show, Eq, Generic)

instance Validity TimeBlock
