{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Data.Gen where

import Data.Time

import Test.QuickCheck

import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()

import Smos.Data

instance GenUnchecked SmosFile

instance GenValid SmosFile where
    genValid = genValidStructurallyWithoutExtraChecking
    shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked a => GenUnchecked (ForYaml a)

instance GenValid a => GenValid (ForYaml a) where
    genValid = genValidStructurallyWithoutExtraChecking
    shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked Entry

instance GenValid Entry where
    genValid = genValidStructurally
    shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked Header

instance GenValid Header where
    genValid = genValidStructurally
    shrinkValid = shrinkValidStructurally

instance GenUnchecked Contents

instance GenValid Contents where
    genValid = genValidStructurally
    shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked PropertyName

instance GenValid PropertyName where
    genValid = genValidStructurally
    shrinkValid = shrinkValidStructurally

instance GenUnchecked PropertyValue

instance GenValid PropertyValue where
    genValid = genValidStructurally
    shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked TimestampName

instance GenValid TimestampName where
    genValid = genValidStructurally
    shrinkValid = shrinkValidStructurally

instance GenUnchecked Timestamp

instance GenValid Timestamp where
    genValid = genValidStructurally
    shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked TodoState

instance GenValid TodoState where
    genValid = genValidStructurally
    shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked StateHistory

instance GenValid StateHistory where
    genValid = genValidStructurally
    shrinkValid = shrinkValidStructurally

instance GenUnchecked StateHistoryEntry where
    shrinkUnchecked (StateHistoryEntry mts ts) =
        StateHistoryEntry <$> shrinkValid mts <*> pure ts

instance GenValid StateHistoryEntry where
    genValid = genValidStructurally
    shrinkValid = shrinkUnchecked

instance GenUnchecked Tag

instance GenValid Tag where
    genValid = genValidStructurally
    shrinkValid = shrinkValidStructurally

instance GenUnchecked Logbook

instance GenValid Logbook where
    genValid =
        let genPositiveNominalDiffTime =
                (realToFrac . abs) <$> (genValid :: Gen Rational)
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
                                    (p:_) ->
                                        sized $ \m -> do
                                            (a, b) <- genSplit m
                                            ndt1 <-
                                                resize
                                                    a
                                                    genPositiveNominalDiffTime
                                            ndt2 <-
                                                resize
                                                    b
                                                    genPositiveNominalDiffTime
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
         in oneof
                [ LogClosed <$> listOfLogbookEntries
                , do lbes <- listOfLogbookEntries
                     l <-
                         case lbes of
                             [] -> genValid
                             (lbe:_) -> do
                                 ndt <- genPositiveNominalDiffTime
                                 pure $ addUTCTime ndt $ logbookEntryEnd lbe
                     pure $ LogOpen l lbes
                ]

instance GenUnchecked LogbookEntry where
    shrinkUnchecked _ = [] -- There's no point.

instance GenValid LogbookEntry where
    genValid =
        sized $ \n -> do
            (a, b) <- genSplit n
            start <- resize a genValid
            ndt <- resize b $ (realToFrac . abs) <$> (genValid :: Gen Rational)
            let end = addUTCTime ndt start
            pure LogbookEntry {logbookEntryStart = start, logbookEntryEnd = end}
