{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Data.Gen where

import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()

import Smos.Data

instance GenUnchecked SmosFile

instance GenValid SmosFile

instance GenUnchecked a => GenUnchecked (ForYaml a)

instance GenValid a => GenValid (ForYaml a)

instance GenUnchecked Entry

instance GenValid Entry where
    genValid =
        Entry <$> genValid <*> genValid <*> genValid <*> genValid <*> genValid <*>
        genValid <*>
        genValid

instance GenUnchecked Header

instance GenValid Header

instance GenUnchecked Contents

instance GenValid Contents

instance GenUnchecked PropertyName

instance GenValid PropertyName

instance GenUnchecked PropertyValue

instance GenValid PropertyValue

instance GenUnchecked TimestampName

instance GenValid TimestampName

instance GenUnchecked Timestamp

instance GenValid Timestamp

instance GenUnchecked TodoState

instance GenValid TodoState

instance GenUnchecked StateHistory

instance GenValid StateHistory

instance GenUnchecked StateHistoryEntry

instance GenValid StateHistoryEntry

instance GenUnchecked Tag

instance GenValid Tag

instance GenUnchecked Logbook

instance GenValid Logbook

instance GenUnchecked LogbookEntry

instance GenValid LogbookEntry
