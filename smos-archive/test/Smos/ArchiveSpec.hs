{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.ArchiveSpec
  ( spec
  ) where

import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Semigroup
import Data.Time
import Data.Tree

import Test.Hspec
import Test.QuickCheck
import Test.Validity

import Smos.Data
import Smos.Data.Gen ()

import Smos.Archive

spec :: Spec
spec =
  describe "prepareToArchive" $ do
    it "produces valid smos files" $ producesValidsOnValids2 prepareToArchive
    it "produces files with no running clocks" $
      forAllValid $ \sf ->
        forAll (genLargeEnoughTime sf) $ \t ->
          let prepped = prepareToArchive t sf
           in allClosed prepped
    it "produces files with no un-done tasks" $
      forAllValid $ \sf ->
        forAll (genLargeEnoughTime sf) $ \t ->
          let prepped = prepareToArchive t sf
           in allDone prepped

allClosed :: SmosFile -> Bool
allClosed = all (all (logbookClosed . entryLogbook)) . smosFileForest

allDone :: SmosFile -> Bool
allDone = all (all (go . entryState)) . smosFileForest
  where
    go :: Maybe TodoState -> Bool
    go = maybe True isDone

genLargeEnoughTime :: SmosFile -> Gen UTCTime
genLargeEnoughTime sf =
  let mMax = NE.nonEmpty . concatMap (mapMaybe go . flatten) $ smosFileForest sf
   in case mMax of
        Nothing -> genValid
        Just us -> genValid `suchThat` (> getMax (sconcat us))
  where
    go :: Entry -> Maybe (Max UTCTime)
    go e = goL (entryLogbook e) <> goTSH (entryStateHistory e)
    goTSH :: StateHistory -> Maybe (Max UTCTime)
    goTSH (StateHistory es) = sconcat <$> NE.nonEmpty (map goSHE es)
    goSHE :: StateHistoryEntry -> Max UTCTime
    goSHE = Max . stateHistoryEntryTimestamp
    goL :: Logbook -> Maybe (Max UTCTime)
    goL =
      \case
        LogOpen u _ -> Just (Max u)
        LogClosed _ -> Nothing
