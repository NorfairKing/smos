{-# LANGUAGE DeriveGeneric #-}

module Smos.Report.Entry where

import GHC.Generics (Generic)

import Data.Validity

import Smos.Data

import Smos.Report.Path

data EntryEntry = EntryEntry
    { entryEntryFilePath :: RootedPath
    , entryEntryEntry :: Entry -- Lol
    } deriving (Show, Eq, Generic)

instance Validity EntryEntry

makeEntryEntry :: RootedPath -> Entry -> EntryEntry
makeEntryEntry rp e = EntryEntry{entryEntryFilePath=rp,entryEntryEntry=e}
