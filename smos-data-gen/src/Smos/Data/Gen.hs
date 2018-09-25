{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Data.Gen where

import Data.List
import qualified Data.Map as M
import Data.Time

import Test.QuickCheck

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
        filter (/= entry) . nub $
        emptyEntry :
        map ($ entry) entryResetFuncs ++ genericShrinkUnchecked entry

instance GenValid Entry where
    genValid = genValidStructurally

entryResetFuncs :: [(Entry -> Entry)]
entryResetFuncs =
    map (foldl (.) id) $
    partitions
        [ (\e -> e {entryHeader = emptyHeader})
        , (\e -> e {entryContents = Nothing})
        , (\e -> e {entryTimestamps = M.empty})
        , (\e -> e {entryProperties = M.empty})
        , (\e -> e {entryStateHistory = StateHistory []})
        , (\e -> e {entryTags = []})
        , (\e -> e {entryLogbook = LogClosed []})
        ]

partitions :: [a] -> [[a]]
partitions [] = [[]]
partitions (a:as) = do
    p <- partitions as
    [p, a : p]

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
    genValid =
        let genPositiveNominalDiffTime = (realToFrac . abs) <$>
                                            (genValid :: Gen Rational)
            listOfLogbookEntries =
                sized $ \n -> do
                    ss <- arbPartition n
                    let go [] = pure []
                        go (s:rest) = do
                            lbes <- go rest
                            cur <-
                                resize s $
                                case lbes of
                                    [] -> genValid
                                    (p:_) -> do
                                        ndt1 <- genPositiveNominalDiffTime
                                        ndt2 <- genPositiveNominalDiffTime
                                        let start =
                                                addUTCTime
                                                    ndt1
                                                    (logbookEntryEnd p)
                                            end = addUTCTime ndt2 start
                                        pure $
                                            LogbookEntry
                                            { logbookEntryStart = start
                                            , logbookEntryEnd = end
                                            }
                            pure $ cur : lbes
                    go ss
        in oneof [LogClosed <$> listOfLogbookEntries, do
            lbes <- listOfLogbookEntries
            l <- case lbes of
                [] -> genValid
                (lbe:_) -> do
                    ndt <-genPositiveNominalDiffTime
                    pure $ addUTCTime ndt $ logbookEntryEnd lbe
            pure $ LogOpen l lbes
            ]

instance GenUnchecked LogbookEntry

instance GenValid LogbookEntry where
    genValid = do
        start <- genValid
        ndt <- (realToFrac . abs) <$> (genValid :: Gen Rational)
        let end = addUTCTime ndt start
        pure LogbookEntry {logbookEntryStart = start, logbookEntryEnd = end}
