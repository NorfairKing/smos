{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Smos.UndoSpec
  ( spec,
  )
where

import Control.Monad
import Data.Maybe
import GHC.Generics (Generic)
import Smos.Undo
import Smos.Undo.Gen ()
import Test.Hspec
import Test.QuickCheck
import Test.Validity

spec :: Spec
spec = do
  genValidSpec @(Undo Int Int)
  genValidSpec @(UndoStack Int Int)
  describe "emptyUndoStack" $ it "is valid" $ shouldBeValid (emptyUndoStack @Int @Int)
  describe "undoStackPush" $ it "produces valid undo stacks" $ producesValidsOnValids2 (undoStackPush @Int @Int)
  describe "undoStackUndo" $ do
    it "produces valid results" $ producesValidsOnValids (undoStackUndo @Int @Int)
    it "is the inverse of undoStackRedo for the undo stack"
      $ forAllValid
      $ \us -> cover 50 (isJust $ composePops undoStackRedo undoStackUndo us) "non-trivial" $
        case undoStackRedo @Int @Int us of
          Nothing -> pure () -- Stack too shont to check anything.
          Just (_, us') -> case undoStackUndo us' of
            Nothing -> pure () -- Stack too shont to check anything.
            Just (_, us'') -> us'' `shouldBe` us
    it "is the inverse of undoStackRedo for IndoRedo"
      $ forAllValid
      $ \is -> forAllValid $ \us -> cover 50 (isJust $ composePops undoStackRedo undoStackUndo us) "non-trivial" $
        case undoStackRedo us of
          Nothing -> pure () -- Stack too shont to check anything.
          Just (ua, us') -> case undoStackUndo us' of
            Nothing -> pure () -- Stack too shont to check anything.
            Just (ra, us'') -> applyIntUndo ra (applyIntRedo ua is) `shouldBe` is
  describe "undoStackRedo" $ do
    it "produces valid results" $
      producesValidsOnValids (undoStackRedo @Int @Int)
    it "is the inverse of undoStackUndo for the undo stack"
      $ forAllValid
      $ \us -> cover 50 (isJust $ composePops undoStackUndo undoStackRedo us) "non-trivial" $
        case undoStackUndo @Int @Int us of
          Nothing -> pure () -- Stack too shont to check anything.
          Just (_, us') -> case undoStackRedo us' of
            Nothing -> pure () -- Stack too shont to check anything.
            Just (_, us'') -> us'' `shouldBe` us
    it "is the inverse of undoStackUndo for IndoUndo"
      $ forAllValid
      $ \is -> forAllValid $ \us -> cover 50 (isJust $ composePops undoStackUndo undoStackRedo us) "non-trivial" $
        case undoStackUndo us of
          Nothing -> pure () -- Stack too shont to check anything.
          Just (ua, us') -> case undoStackRedo us' of
            Nothing -> pure () -- Stack too shont to check anything.
            Just (ra, us'') -> applyIntRedo ra (applyIntUndo ua is) `shouldBe` is

composePops :: (a -> Maybe (b, c)) -> (c -> Maybe (d, e)) -> a -> Maybe e
composePops func1 func2 a = do
  (_, c) <- func1 a
  (_, e) <- func2 c
  pure e

data IntUndo = UndoAdd Int | UndoSub Int
  deriving (Show, Eq, Generic)

instance Validity IntUndo

instance GenValid IntUndo where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

data IntRedo = RedoAdd Int | RedoSub Int
  deriving (Show, Eq, Generic)

instance Validity IntRedo

instance GenValid IntRedo where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

applyIntRedo :: IntRedo -> Int -> Int
applyIntRedo = \case
  RedoAdd i -> (\x -> x + i)
  RedoSub i -> (\x -> x - i)

applyIntUndo :: IntUndo -> Int -> Int
applyIntUndo = \case
  UndoAdd i -> (\x -> x - i)
  UndoSub i -> (\x -> x + i)
