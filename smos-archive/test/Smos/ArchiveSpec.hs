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
        forAll (genLargeEnoughTime sf) $ \t -> do
          let prepped = prepareToArchive t sf
          any (any (logbookOpen . entryLogbook)) (smosFileForest prepped) `shouldBe` False
    it "produces files with no un-done tasks" $
      forAllValid $ \sf ->
        forAll (genLargeEnoughTime sf) $ \t -> do
          let prepped = prepareToArchive t sf
          any (any (not . isDone . entryState)) (smosFileForest prepped) `shouldBe` False

genLargeEnoughTime :: SmosFile -> Gen UTCTime
genLargeEnoughTime sf =
  let mMax =
        NE.nonEmpty . concatMap (map Max . mapMaybe (go . entryLogbook) . flatten) $
        smosFileForest sf
   in case mMax of
        Nothing -> genValid
        Just us -> genValid `suchThat` (> getMax (sconcat us))
  where
    go :: Logbook -> Maybe UTCTime
    go =
      \case
        LogOpen u _ -> Just u
        LogClosed _ -> Nothing
