{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Smos.UndoSpec
  ( spec,
  )
where

import Data.Maybe
import GHC.Generics (Generic)
import Smos.Undo
import Smos.Undo.Gen ()
import Test.Syd
import Test.QuickCheck
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @(UndoStack Int)
  describe "emptyUndoStack" $ it "is valid" $ shouldBeValid (emptyUndoStack @Int)
  describe "undoStackPush" $ do
    it "produces valid undo stacks" $ producesValidsOnValids2 (undoStackPush @Int)
    it "the redo stack is empty after pushing a new undo" $
      forAllValid $ \u -> forAllValid $ \us ->
        let us' = undoStackPush @Int u us
         in case undoStackRedo us' of
              Nothing -> pure () -- Great
              Just _ -> expectationFailure "The undo stack should have been empty."
  describe "undoStackUndo" $ do
    it "produces valid results" $ producesValidsOnValids (undoStackUndo @Int)
    it "is the inverse of undoStackRedo for the undo stack" $
      forAllValid $
        \us -> cover 50 (isJust $ composePops undoStackRedo undoStackUndo us) "non-trivial" $
          case undoStackRedo @Int us of
            Nothing -> pure () -- Stack too shont to check anything.
            Just (_, us') -> case undoStackUndo us' of
              Nothing -> pure () -- Stack too shont to check anything.
              Just (_, us'') -> us'' `shouldBe` us
    it "is the inverse of undoStackRedo for IndoRedo" $
      forAllValid $
        \is -> forAllValid $ \us -> cover 50 (isJust $ composePops undoStackRedo undoStackUndo us) "non-trivial" $
          case undoStackRedo us of
            Nothing -> pure () -- Stack too shont to check anything.
            Just (ua, us') -> case undoStackUndo us' of
              Nothing -> pure () -- Stack too shont to check anything.
              Just (ra, _) -> applyIntUndo ra (applyIntRedo ua is) `shouldBe` is
  describe "undoStackRedo" $ do
    it "produces valid results" $
      producesValidsOnValids (undoStackRedo @Int)
    it "is the inverse of undoStackUndo for the undo stack" $
      forAllValid $
        \us -> cover 50 (isJust $ composePops undoStackUndo undoStackRedo us) "non-trivial" $
          case undoStackUndo @Int us of
            Nothing -> pure () -- Stack too shont to check anything.
            Just (_, us') -> case undoStackRedo us' of
              Nothing -> pure () -- Stack too shont to check anything.
              Just (_, us'') -> us'' `shouldBe` us
    it "is the inverse of undoStackUndo for IndoUndo" $
      forAllValid $
        \is -> forAllValid $ \us -> cover 50 (isJust $ composePops undoStackUndo undoStackRedo us) "non-trivial" $
          case undoStackUndo us of
            Nothing -> pure () -- Stack too shont to check anything.
            Just (ua, us') -> case undoStackRedo us' of
              Nothing -> pure () -- Stack too shont to check anything.
              Just (ra, _) -> applyIntRedo ra (applyIntUndo ua is) `shouldBe` is

composePops :: (a -> Maybe (b, c)) -> (c -> Maybe (d, e)) -> a -> Maybe e
composePops func1 func2 a = do
  (_, c) <- func1 a
  (_, e) <- func2 c
  pure e

data IntAction = Add Int | Sub Int
  deriving (Show, Eq, Generic)

instance Validity IntAction

instance GenValid IntAction where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

applyIntRedo :: IntAction -> Int -> Int
applyIntRedo = \case
  Add i -> (\x -> x + i)
  Sub i -> (\x -> x - i)

applyIntUndo :: IntAction -> Int -> Int
applyIntUndo = \case
  Add i -> (\x -> x - i)
  Sub i -> (\x -> x + i)
