{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Data.Gen where

import qualified Data.Map as M

import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()

import Smos.Data

instance GenUnchecked SmosFile where
    shrinkUnchecked (SmosFile a) = SmosFile <$> shrinkUnchecked a

instance GenValid SmosFile where
    genValid = SmosFile <$> genValid

instance GenUnchecked a => GenUnchecked (ForYaml a) where
    shrinkUnchecked (ForYaml a) = ForYaml <$> shrinkUnchecked a

instance GenValid a => GenValid (ForYaml a) where
    genValid = ForYaml <$> genValid

instance GenUnchecked Entry where
    shrinkUnchecked entry =
        let funcs =
                [ (\e -> e {entryHeader = emptyHeader})
                , (\e -> e {entryContents = Nothing})
                , (\e -> e {entryTimestamps = M.empty})
                , (\e -> e {entryProperties = M.empty})
                , (\e -> e {entryStateHistory = StateHistory []})
                , (\e -> e {entryTags = []})
                , (\e -> e {entryLogbook = LogClosed []})
                ]
            funcss = map (foldl (.) id) $ partitions funcs
        in emptyEntry : map ($ entry) funcss ++ genericShrinkUnchecked entry

partitions :: [a] -> [[a]]
partitions [] = [[]]
partitions (a:as) = do
    p <- partitions as
    [a : p, p]

instance GenValid Entry where
    genValid = genValidStructurally

instance GenUnchecked Header

instance GenValid Header where
    genValid = genValidStructurally

instance GenUnchecked Contents

instance GenValid Contents where
    genValid = genValidStructurally

instance GenUnchecked PropertyName

instance GenValid PropertyName where
    genValid = genValidStructurally

instance GenUnchecked PropertyValue

instance GenValid PropertyValue where
    genValid = genValidStructurally

instance GenUnchecked TimestampName

instance GenValid TimestampName where
    genValid = genValidStructurally

instance GenUnchecked Timestamp

instance GenValid Timestamp where
    genValid = genValidStructurally

instance GenUnchecked TodoState

instance GenValid TodoState where
    genValid = genValidStructurally

instance GenUnchecked StateHistory

instance GenValid StateHistory where
    genValid = genValidStructurally

instance GenUnchecked StateHistoryEntry

instance GenValid StateHistoryEntry where
    genValid = genValidStructurally

instance GenUnchecked Tag

instance GenValid Tag where
    genValid = genValidStructurally

instance GenUnchecked Logbook

instance GenValid Logbook where
    genValid = genValidStructurally

instance GenUnchecked LogbookEntry

instance GenValid LogbookEntry where
    genValid = genValidStructurally
