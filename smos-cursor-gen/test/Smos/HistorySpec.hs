{-# LANGUAGE TypeApplications #-}

module Smos.HistorySpec
  ( spec,
  )
where

import Control.Monad
import Cursor.List.NonEmpty
import Data.Maybe
import GHC.Generics (Generic)
import Smos.History
import Smos.History.Gen
import Test.Hspec
import Test.QuickCheck
import Test.Validity
import Test.Validity.Optics

spec :: Spec
spec = do
  genValidSpec @(History Int)
  lensSpecOnValid (historyNonEmptyCursorL @Int)
  lensSpecOnValid (historyPresentL @Int)
  describe "singletonHistory" $ it "produces valid histories" $ producesValidsOnValids (startingHistory @Int)
  describe "historyPresent" $ it "produces valid states" $ producesValidsOnValids (historyPresent @Int)
  describe "historyPush" $ do
    it "produces valid undo stacks" $ producesValidsOnValids2 (historyPush @Int)
    it "the future is empty after pushing a new undo" $ forAllValid $ \u -> forAllValid $ \us ->
      let us' = historyPush @Int u us
       in case historyRedo us' of
            Nothing -> pure () -- Great
            Just _ -> expectationFailure "The undo stack should have been empty."
  describe "historyUndo" $ do
    it "produces valid results" $ producesValidsOnValids (historyUndo @Int)
    it "is the inverse of historyRedo for the history"
      $ forAllValid
      $ \us -> cover 50 (isJust $ (historyRedo >=> historyUndo) us) "non-trivial" $
        case historyRedo @Int us of
          Nothing -> pure () -- Stack too shont to check anything.
          Just us' -> case historyUndo us' of
            Nothing -> pure () -- Stack too shont to check anything.
            Just us'' -> us'' `shouldBe` us
  describe "historyRedo" $ do
    it "produces valid results" $
      producesValidsOnValids (historyRedo @Int)
    it "is the inverse of historyUndo for the undo stack"
      $ forAllValid
      $ \us -> cover 50 (isJust $ (historyUndo >=> historyRedo) us) "non-trivial" $
        case historyUndo @Int us of
          Nothing -> pure () -- Stack too shont to check anything.
          Just us' -> case historyRedo us' of
            Nothing -> pure () -- Stack too shont to check anything.
            Just us'' -> us'' `shouldBe` us
  describe "historyMod" $ do
    let f i = historyMod (+ (i :: Int))
    it "produces valid results for addition" $ forAllValid $ \i -> producesValidsOnValids (historyMod (+ (i :: Int)))
    it "produces a history with one longer undo stack" $ forAllValid $ \i -> forAllValid $ \h ->
      let h' = historyMod (+ (i :: Int)) h
          undoStackLength = length . nonEmptyCursorPrev . historyNonEmptyCursor
       in undoStackLength h' `shouldBe` (undoStackLength h + 1)
  describe "historyModM" $ do
    let f i =
          historyModM
            ( \j -> case (j :: Int) of
                0 -> Nothing
                _ -> Just $ i `div` j
            )
    it "produces valid results for division with Maybe" $ forAllValid $ \i ->
      producesValidsOnValids (f i)
    it "produces a history with one longer undo stack" $ forAllValid $ \i -> forAllValid $ \h -> do
      let mh' = f i h
          undoStackLength = length . nonEmptyCursorPrev . historyNonEmptyCursor
      case mh' of
        Nothing -> pure ()
        Just h' -> undoStackLength h' `shouldBe` (undoStackLength h + 1)
