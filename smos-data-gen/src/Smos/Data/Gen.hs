{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Data.Gen where

import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.Text
import Data.GenValidity.Time ()
import Data.List
import Data.Time
import Smos.Data
import Test.QuickCheck

instance GenValid SmosFile where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenUnchecked a => GenUnchecked (ForYaml a)

instance GenValid a => GenValid (ForYaml a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Entry where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Header where
  genValid = Header <$> genTextBy genHeaderChar
  shrinkValid = shrinkValidStructurally

genHeaderChar :: Gen Char
genHeaderChar = choose (minBound, maxBound) `suchThat` validHeaderChar

instance GenValid Contents where
  genValid = Contents <$> genTextBy (choose (minBound, maxBound) `suchThat` validContentsChar)
  shrinkValid = shrinkValidStructurally

instance GenValid PropertyName where
  genValid =
    PropertyName <$> genTextBy (choose (minBound, maxBound) `suchThat` validPropertyNameChar)
  shrinkValid = shrinkValidStructurally

instance GenValid PropertyValue where
  genValid =
    PropertyValue <$> genTextBy (choose (minBound, maxBound) `suchThat` validPropertyValueChar)
  shrinkValid = shrinkValidStructurally

instance GenValid TimestampName where
  genValid =
    TimestampName <$> genTextBy (choose (minBound, maxBound) `suchThat` validTimestampNameChar)
  shrinkValid = shrinkValidStructurally

instance GenUnchecked Timestamp

instance GenValid Timestamp where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid TodoState where
  genValid = TodoState <$> genTextBy (choose (minBound, maxBound) `suchThat` validTodoStateChar)
  shrinkValid = shrinkValidStructurally

instance GenValid StateHistory where
  genValid = StateHistory . sort <$> genValid
  shrinkValid =
    fmap StateHistory .
    shrinkList (\(StateHistoryEntry mts ts) -> StateHistoryEntry <$> shrinkValid mts <*> pure ts) .
    unStateHistory

instance GenValid StateHistoryEntry where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Tag where
  genValid = Tag <$> genTextBy genTagChar
  shrinkValid = shrinkValidStructurally

genTagChar :: Gen Char
genTagChar = choose (minBound, maxBound) `suchThat` validTagChar

instance GenUnchecked Logbook

instance GenValid Logbook where
  genValid =
    let genPositiveNominalDiffTime = realToFrac . abs <$> (genValid :: Gen Rational)
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
                          ndt1 <- resize a genPositiveNominalDiffTime
                          ndt2 <- resize b genPositiveNominalDiffTime
                          let start = addUTCTime ndt1 (logbookEntryEnd p)
                              end = addUTCTime ndt2 start
                          pure $ LogbookEntry {logbookEntryStart = start, logbookEntryEnd = end}
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
      ndt <- resize b $ realToFrac . abs <$> (genValid :: Gen Rational)
      let end = addUTCTime ndt start
      pure LogbookEntry {logbookEntryStart = start, logbookEntryEnd = end}
