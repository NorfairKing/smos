{-# LANGUAGE TypeApplications #-}

module Smos.HistorySpec
  ( spec,
  )
where

import Control.Monad
import Cursor.List.NonEmpty.Gen ()
import Data.Maybe
import Smos.History
import Smos.History.Gen ()
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Lens

spec :: Spec
spec = do
  genValidSpec @(History Int)
  lensSpecOnValid (historyNonEmptyCursorL @Int)
  lensSpecOnValid (historyPresentL @Int)
  describe "singletonHistory" $ it "produces valid histories" $ producesValidsOnValids (startingHistory @Int)
  describe "historyPresent" $ it "produces valid states" $ producesValidsOnValids (historyPresent @Int)
  describe "historyPush" $ do
    it "produces valid undo stacks" $ producesValidsOnValids2 (historyPush @Int)
    it "the future is empty after pushing a new undo" $
      forAllValid $ \u -> forAllValid $ \us ->
        let us' = historyPush @Int u us
         in case historyRedo us' of
              Nothing -> pure () -- Great
              Just _ -> expectationFailure "The undo stack should have been empty."
    it "replaces the current present" $
      forAllValid $ \i -> forAllValid $ \h ->
        let h' = historyPush @Int i h
         in historyPresent h' `shouldBe` i
  describe "historyUndo" $ do
    it "produces valid results" $ producesValidsOnValids (historyUndo @Int)
    it "is the inverse of historyRedo for the history" $
      forAllValid $
        \us -> cover 50 (isJust $ (historyRedo >=> historyUndo) us) "non-trivial" $
          case historyRedo @Int us of
            Nothing -> pure () -- Stack too short to check anything.
            Just us' -> case historyUndo us' of
              Nothing -> pure () -- Stack too short to check anything.
              Just us'' -> us'' `shouldBe` us
  describe "historyRedo" $ do
    it "produces valid results" $
      producesValidsOnValids (historyRedo @Int)
    it "is the inverse of historyUndo for the undo stack" $
      forAllValid $
        \us -> cover 50 (isJust $ (historyUndo >=> historyRedo) us) "non-trivial" $
          case historyUndo @Int us of
            Nothing -> pure () -- Stack too short to check anything.
            Just us' -> case historyRedo us' of
              Nothing -> pure () -- Stack too short to check anything.
              Just us'' -> us'' `shouldBe` us
  describe "historyMod" $ do
    let f i = historyMod (+ (i :: Int))
    it "produces valid results for addition" $ forAllValid $ \i -> producesValidsOnValids (f i)
    it "produces a history with one longer undo stack" $
      forAllValid $ \i -> forAllValid $ \h -> do
        let h' = f i h
        historyUndoLength h' `shouldBe` (historyUndoLength h + 1)
    it "produces a history with a 0 length redo stack" $
      forAllValid $ \i -> forAllValid $ \h -> do
        let h' = f i h
        historyRedoLength h' `shouldBe` 0
  describe "historyModM" $ do
    let f i =
          historyModM
            ( \j -> case (j :: Int) of
                0 -> Nothing
                _ -> Just $ i - j
            )
    it "produces valid results for subtraction with Maybe" $
      forAllValid $ \i ->
        producesValidsOnValids (f i)
    it "produces a history with one longer undo stack" $
      forAllValid $ \i -> forAllValid $ \h ->
        case f i h of
          Nothing -> pure ()
          Just h' -> historyUndoLength h' `shouldBe` (historyUndoLength h + 1)
    it "produces a history with a 0 length redo stack" $
      forAllValid $ \i -> forAllValid $ \h ->
        case f i h of
          Nothing -> pure ()
          Just h' -> historyRedoLength h' `shouldBe` 0
  describe "historyUndoLength" $ it "produces valid words" $ producesValidsOnValids (historyUndoLength @Int)
  describe "historyRedoLength" $ it "produces valid words" $ producesValidsOnValids (historyRedoLength @Int)
  describe "historyForget" $ it "produces valid histories" $ producesValidsOnValids (historyForgetLatest @Int)
