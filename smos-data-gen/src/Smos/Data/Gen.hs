{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Data.Gen where

import Data.GenValidity
import Data.GenValidity.Containers
import Data.GenValidity.Text
import Data.GenValidity.Time ()
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.SemVer as Version
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time
import Data.Time.Zones
import Data.Time.Zones.All
import Data.Word
import Smos.Data
import Test.QuickCheck

instance (GenValid a) => GenValid (Versioned a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Version where
  shrinkValid _ = [] -- No shrinking for now
  genValid =
    let genComponent = fromIntegral <$> (genValid :: Gen Word32) -- This doesn't generate version numbers >2^32 but that's fine.
     in version <$> genComponent <*> genComponent <*> genComponent <*> pure [] <*> pure [] -- No identifiers for now

instance GenValid SmosFile where
  genValid = SmosFile <$> genValid
  shrinkValid sf = do
    f <- shrinkValid (smosFileForest sf)
    pure $ sf {smosFileForest = f}

genInterestingSmosFile :: Gen SmosFile
genInterestingSmosFile = makeSmosFile <$> genInterestingForest

genInterestingForest :: Gen (Forest Entry)
genInterestingForest =
  frequency
    [ (1, pure []),
      (9, NE.toList <$> genNonEmptyOf genInterestingEntryTree)
    ]

genInterestingEntryTree :: Gen (Tree Entry)
genInterestingEntryTree = genTreeOf genInterestingEntry

instance GenValid Entry where
  genValid =
    oneof
      [ sized $ \size -> do
          (a, b, c, d, e, f, g) <- genSplit7 size
          entryHeader <- resize a genValid
          entryContents <- resize b genValid
          entryTimestamps <- resize c genValid
          entryProperties <- resize d genValid
          entryStateHistory <- resize e genValid
          entryTags <- resize f genValid
          entryLogbook <- resize g genValid
          pure Entry {..},
        genInterestingEntry
      ]
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

genInterestingEntry :: Gen Entry
genInterestingEntry =
  sized $ \size -> do
    (a, b, c, d, e, f, g) <- genSplit7 size
    entryHeader <- resize a genValid
    entryContents <- resize b genValid
    entryTimestamps <- resize c genInterestingTimestamps
    entryProperties <- resize d genInterestingProperties
    entryStateHistory <- resize e genInterestingStateHistory
    entryTags <- resize f genInterestingTags
    entryLogbook <- resize g genValid
    pure Entry {..}

genInterestingProperties :: Gen (Map PropertyName PropertyValue)
genInterestingProperties =
  genInterestingMapOf $
    (,) <$> genInterestingPropertyName <*> genValid

genInterestingTimestamps :: Gen (Map TimestampName Timestamp)
genInterestingTimestamps =
  genInterestingMapOf $
    (,) <$> genInterestingTimestampName <*> genValid

genInterestingTags :: Gen (Set Tag)
genInterestingTags = genInterestingSetOf genInterestingTag

instance GenValid Header where
  genValid = Header <$> genTextBy genHeaderChar
  shrinkValid = shrinkValidStructurally

genHeaderChar :: Gen Char
genHeaderChar = choose (minBound, maxBound) `suchThat` validHeaderChar

instance GenValid Contents where
  genValid = Contents <$> genTextBy genContentsChar
  shrinkValid = shrinkValidStructurally

genContentsChar :: Gen Char
genContentsChar = choose (minBound, maxBound) `suchThat` validContentsChar

instance GenValid PropertyName where
  genValid =
    oneof
      [ PropertyName <$> genTextBy genPropertyNameChar,
        genInterestingPropertyName
      ]
  shrinkValid = shrinkValidStructurally

genPropertyNameChar :: Gen Char
genPropertyNameChar = choose (minBound, maxBound) `suchThat` validPropertyNameChar

genInterestingPropertyName :: Gen PropertyName
genInterestingPropertyName =
  elements
    [ "assignee",
      "brainpower",
      "client",
      "email_address",
      "goal",
      "phone_number",
      "timewindow",
      "url",
      "waiting_threshold"
    ]

instance GenValid PropertyValue where
  genValid = PropertyValue <$> genTextBy genPropertyValueChar
  shrinkValid = shrinkValidStructurally

genPropertyValueChar :: Gen Char
genPropertyValueChar = choose (minBound, maxBound) `suchThat` validPropertyValueChar

instance GenValid TimestampName where
  genValid =
    oneof
      [ TimestampName <$> genTextBy genTimestampNameChar,
        genInterestingTimestampName
      ]
  shrinkValid = shrinkValidStructurally

genTimestampNameChar :: Gen Char
genTimestampNameChar = choose (minBound, maxBound) `suchThat` validTimestampNameChar

genInterestingTimestampName :: Gen TimestampName
genInterestingTimestampName =
  elements
    [ "AFTER",
      "BEGIN",
      "DEADLINE",
      "END",
      "SCHEDULED"
    ]

instance GenValid Timestamp where
  shrinkValid = shrinkValidStructurally
  genValid =
    oneof
      [ TimestampDay <$> genValid,
        TimestampLocalTime <$> genImpreciseLocalTime
      ]

instance GenValid TodoState where
  genValid =
    oneof
      [ TodoState <$> genTextBy genTodoStateChar,
        genInterestingTodoState
      ]
  shrinkValid = shrinkValidStructurally

genTodoStateChar :: Gen Char
genTodoStateChar = choose (minBound, maxBound) `suchThat` validTodoStateChar

genInterestingTodoState :: Gen TodoState
genInterestingTodoState =
  elements
    [ "TODO",
      "NEXT",
      "STARTED",
      "READY",
      "CANCELLED",
      "DONE",
      "FAILED",
      "WAITING"
    ]

instance GenValid StateHistory where
  genValid =
    oneof
      [ StateHistory . sort <$> genValid,
        genInterestingStateHistory
      ]
  shrinkValid =
    fmap StateHistory
      . shrinkList (\(StateHistoryEntry mts ts) -> StateHistoryEntry <$> shrinkValid mts <*> pure ts)
      . unStateHistory

genInterestingStateHistory :: Gen StateHistory
genInterestingStateHistory = StateHistory . sort <$> genInterestingListOf genInterestingStateHistoryEntry

instance GenValid StateHistoryEntry where
  genValid =
    oneof
      [ genValidStructurallyWithoutExtraChecking,
        genInterestingStateHistoryEntry
      ]
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

genInterestingStateHistoryEntry :: Gen StateHistoryEntry
genInterestingStateHistoryEntry =
  StateHistoryEntry
    <$> genInterestingMaybeOf genInterestingTodoState
    <*> genValid

instance GenValid Tag where
  genValid =
    oneof
      [ Tag <$> genTextBy genTagChar,
        genInterestingTag
      ]
  shrinkValid = shrinkValidStructurally

genTagChar :: Gen Char
genTagChar = choose (minBound, maxBound) `suchThat` validTagChar

genInterestingTag :: Gen Tag
genInterestingTag =
  elements
    [ "code",
      "external",
      "home",
      "offline",
      "online",
      "power",
      "toast",
      "work"
    ]

instance GenValid Logbook where
  genValid =
    let listOfLogbookEntries =
          sized $ \n -> do
            ss <- arbPartition n
            let go [] = pure []
                go (s : rest) = do
                  lbes <- go rest
                  cur <-
                    resize s $
                      case lbes of
                        [] -> genValid
                        (p : _) ->
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
          [ LogClosed <$> listOfLogbookEntries,
            do
              lbes <- listOfLogbookEntries
              l <-
                case lbes of
                  [] -> genImpreciseUTCTime
                  (lbe : _) -> do
                    ndt <- genPositiveNominalDiffTime
                    pure $ addUTCTime ndt $ logbookEntryEnd lbe
              pure $ LogOpen l lbes
          ]

genPositiveNominalDiffTime :: Gen NominalDiffTime
genPositiveNominalDiffTime =
  sized $ \s -> do
    -- Integer multipliers, because these represent seconds.
    multiplier <- elements [1, 10, 100, 1000, 10000]
    base <- realToFrac <$> choose (0, fromIntegral s :: Word)
    pure (base * multiplier)

instance GenValid LogbookEntry where
  genValid =
    sized $ \n -> do
      (a, b) <- genSplit n
      start <- resize a genImpreciseUTCTime
      ndt <- resize b genPositiveNominalDiffTime
      let end = addUTCTime ndt start
      pure
        LogbookEntry
          { logbookEntryStart = start,
            logbookEntryEnd = end
          }
  shrinkValid _ = [] -- There's no point.

genImpreciseUTCTime :: Gen UTCTime
genImpreciseUTCTime = localTimeToUTC utc <$> genImpreciseLocalTime

genImpreciseLocalTime :: Gen LocalTime
genImpreciseLocalTime = LocalTime <$> genValid <*> genImpreciseTimeOfDay

genImpreciseTimeOfDay :: Gen TimeOfDay
genImpreciseTimeOfDay =
  TimeOfDay
    <$> choose (0, 23)
    <*> choose (0, 59)
    <*> (fromIntegral <$> (choose (0, 60) :: Gen Int))

instance GenValid TZ where
  genValid = tzByLabel <$> genValid
  shrinkValid _ = [] -- Not sure if this is possible

instance GenValid TZLabel

genInterestingSetOf :: (Ord v) => Gen v -> Gen (Set v)
genInterestingSetOf g = S.fromList <$> genInterestingListOf g

genInterestingMapOf :: (Ord k) => Gen (k, v) -> Gen (Map k v)
genInterestingMapOf g = M.fromList <$> genInterestingListOf g

genInterestingListOf :: Gen v -> Gen [v]
genInterestingListOf g =
  frequency
    [ (1, pure []),
      (9, NE.toList <$> genNonEmptyOf g)
    ]

genInterestingMaybeOf :: Gen v -> Gen (Maybe v)
genInterestingMaybeOf g =
  frequency
    [ (1, pure Nothing),
      (9, Just <$> g)
    ]
