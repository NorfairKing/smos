{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.TagsSpec where

import Control.Applicative
import Cursor.Types
import Smos.Cursor.Tags
import Smos.Cursor.Tags.Gen ()
import Smos.Data.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @TagsCursor
  describe "makeTagsCursor" $ it "produces valid cursors" $ producesValidsOnValids makeTagsCursor
  describe "singletonTagsCursor" $
    it "produces valid cursors" $
      producesValidsOnValids singletonTagsCursor
  describe "rebuildTagsCursor" $ do
    it "produces valid cursors" $ producesValidsOnValids rebuildTagsCursor
    it "is the inverse of makeTagsCursor" $ inverseFunctionsOnValid makeTagsCursor rebuildTagsCursor
  describe "tagsCursorSetTag" $ do
    it "produces valid cursors" $ producesValidsOnValids2 tagsCursorSetTag
    it "ensures that the tag is set afterward" $
      forAllValid $
        \t ->
          forAllValid $ \mtc ->
            case tagsCursorSetTag t mtc <|> mtc of
              Nothing -> expectationFailure "Setting a tag on Nothing should not have failed."
              Just tc -> tc `shouldSatisfy` (elem t . rebuildTagsCursor)
  describe "tagsCursorUnsetTag" $ do
    it "produces valid cursors" $ producesValidsOnValids2 tagsCursorUnsetTag
    it "is vaguely the inverse of tagsCursorSetTag" $
      forAllValid $
        \t ->
          inverseFunctionsIfSucceedOnValid
            (tagsCursorSetTag t . Just . makeTagsCursor)
            (fmap rebuildTagsCursor . dullMDelete . tagsCursorUnsetTag t)
    it "ensures that the tag is unset afterward" $
      forAllValid $
        \t ->
          forAllValid $ \tc ->
            case tagsCursorUnsetTag t tc of
              Nothing -> tc `shouldSatisfy` (notElem t . rebuildTagsCursor)
              Just Deleted -> pure () -- Definitely unset then.
              Just (Updated tc') -> tc' `shouldSatisfy` (notElem t . rebuildTagsCursor)
  describe "tagsCursorToggleTag" $ do
    it "produces valid cursors" $ producesValidsOnValids2 tagsCursorToggleTag
    it "toggles the given tag" $
      forAllValid $
        \t ->
          forAllValid $ \mtc ->
            case tagsCursorToggleTag t mtc of
              Deleted ->
                case mtc of
                  Nothing -> expectationFailure "Should not have deleted Nothing."
                  Just tc -> tc `shouldSatisfy` (elem t . rebuildTagsCursor)
              Updated tc' ->
                case mtc of
                  Nothing -> tc' `shouldSatisfy` (elem t . rebuildTagsCursor)
                  Just tc ->
                    if elem t $ rebuildTagsCursor tc
                      then tc' `shouldSatisfy` (notElem t . rebuildTagsCursor)
                      else tc' `shouldSatisfy` (elem t . rebuildTagsCursor)
  describe "tagsCursorInsert" $ do
    it "produces valid cursors when inserting '\n'" $
      forAllValid $
        \tsc -> shouldBeValid $ tagsCursorInsert '\n' tsc
    it "produces valid cursors when inserting an unsafe character" $
      forAllValid $
        \tsc -> shouldBeValid $ tagsCursorInsert '\55810' tsc
    it "produces valid tags cursors" $ producesValidsOnValids2 tagsCursorInsert
  describe "tagsCursorAppend" $ do
    it "produces valid cursors when inserting '\n'" $
      forAllValid $
        \tsc -> shouldBeValid $ tagsCursorAppend '\n' tsc
    it "produces valid cursors when inserting an unsafe character" $
      forAllValid $
        \tsc -> shouldBeValid $ tagsCursorAppend '\55810' tsc
    it "produces valid tags cursors" $ producesValidsOnValids2 tagsCursorAppend
  describe "tagsCursorInsertTag" $
    it "produces valid tags cursors" $
      producesValidsOnValids2 tagsCursorInsertTag
  describe "tagsCursorAppendTag" $
    it "produces valid tags cursors" $
      producesValidsOnValids2 tagsCursorAppendTag
  describe "tagsCursorInsertAndSelectTag" $
    it "produces valid tags cursors" $
      producesValidsOnValids2 tagsCursorInsertAndSelectTag
  describe "tagsCursorAppendAndSelectTag" $
    it "produces valid tags cursors" $
      producesValidsOnValids2 tagsCursorAppendAndSelectTag
  describe "tagsCursorDelete" $
    it "produces valid tags cursors" $
      producesValidsOnValids tagsCursorDelete
  describe "tagsCursorRemove" $
    it "produces valid tags cursors" $
      producesValidsOnValids tagsCursorRemove
  describe "tagsCursorSelectPrev" $ do
    it "produces valid tags cursors" $ producesValidsOnValids tagsCursorSelectPrev
    it "is the inverse of tagsCursorSelectNext" $
      inverseFunctionsIfSucceedOnValid tagsCursorSelectNext tagsCursorSelectPrev
  describe "tagsCursorSelectNext" $ do
    it "produces valid tags cursors" $ producesValidsOnValids tagsCursorSelectNext
    it "is the inverse of tagsCursorSelectPrev" $
      inverseFunctionsIfSucceedOnValid tagsCursorSelectPrev tagsCursorSelectNext
  describe "tagsCursorSelectOrCreatePrev" $
    it "produces valid tags cursors" $
      producesValidsOnValids tagsCursorSelectOrCreatePrev
  describe "tagsCursorSelectOrCreateNext" $
    it "produces valid tags cursors" $
      producesValidsOnValids tagsCursorSelectOrCreateNext
  describe "tagsCursorSelectPrevChar" $ do
    it "produces valid tags cursors" $ producesValidsOnValids tagsCursorSelectPrevChar
    it "is the inverse of tagsCursorSelectNextChar" $
      inverseFunctionsIfSucceedOnValid tagsCursorSelectNextChar tagsCursorSelectPrevChar
  describe "tagsCursorSelectNextChar" $ do
    it "produces valid tags cursors" $ producesValidsOnValids tagsCursorSelectNextChar
    it "is the inverse of tagsCursorSelectPrevChar" $
      inverseFunctionsIfSucceedOnValid tagsCursorSelectPrevChar tagsCursorSelectNextChar
  describe "tagsCursorSelectPrevTag" $
    it "produces valid tags cursors" $
      producesValidsOnValids tagsCursorSelectPrevTag
  describe "tagsCursorSelectNextTag" $
    it "produces valid tags cursors" $
      producesValidsOnValids tagsCursorSelectNextTag
  describe "tagsCursorSelectOrCreatePrevTag" $
    it "produces valid tags cursors" $
      producesValidsOnValids tagsCursorSelectOrCreatePrevTag
  describe "tagsCursorSelectOrCreateNextTag" $
    it "produces valid tags cursors" $
      producesValidsOnValids tagsCursorSelectOrCreateNextTag
