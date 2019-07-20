{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.HelpSpec where

import Data.List

import Test.Hspec
import Test.Validity
import Test.Validity.Optics

import Smos.Data.Gen ()

import Smos.Cursor.Help.Gen ()
import Smos.Types

spec :: Spec
spec = do
  eqSpecOnValid @HelpCursor
  genValidSpec @HelpCursor
  -- Can't test this
  -- describe "makeHelpCursor" $ it "produces valid cursors" $ producesValidsOnValids2 makeHelpCursor
  -- Does not hold?
  -- describe "helpCursorKeySearchBarL" $ lensSpecOnValid helpCursorKeySearchBarL
  describe "searchHelpCursor" $ do
    it "produces valid search results" $ producesValidsOnValids2 searchHelpCursor
    it "selects a subset of the available KeyHelpCursors" $
      forAllValid $ \q ->
        forAllValid $ \khcs -> searchHelpCursor q khcs `shouldSatisfy` (`isSubsequenceOf` khcs)
  describe "helpCursorSelectedKeyHelpCursorsL" $ lensSpecOnValid helpCursorSelectedKeyHelpCursorsL
  describe "helpCursorUp" $ it "produces valid cursors" $ producesValidsOnValids helpCursorUp
  describe "helpCursorDown" $ it "produces valid cursors" $ producesValidsOnValids helpCursorDown
  describe "helpCursorStart" $ it "produces valid cursors" $ producesValidsOnValids helpCursorStart
  describe "helpCursorEnd" $ it "produces valid cursors" $ producesValidsOnValids helpCursorEnd
  describe "helpCursorSelectionL" $ lensSpecOnValid helpCursorSelectionL
  describe "helpCursorSelectHelp" $
    it "produces valid cursors" $ producesValidsOnValids helpCursorSelectHelp
  describe "helpCursorSelectSearch" $
    it "produces valid cursors" $ producesValidsOnValids helpCursorSelectSearch
  describe "helpCursorToggleSelection" $
    it "produces valid cursors" $ producesValidsOnValids helpCursorToggleSelection
  describe "helpCursorInsert" $
    it "produces valid cursors" $ producesValidsOnValids2 helpCursorInsert
  describe "helpCursorAppend" $
    it "produces valid cursors" $ producesValidsOnValids2 helpCursorAppend
  describe "helpCursorRemove" $
    it "produces valid cursors" $ producesValidsOnValids helpCursorRemove
  describe "helpCursorDelete" $
    it "produces valid cursors" $ producesValidsOnValids helpCursorDelete
  eqSpecOnValid @KeyHelpCursor
  genValidSpec @KeyHelpCursor
  describe "combineKeyHelpCursors" $
    it "produces valid cursors" $ producesValidsOnValids combineKeyHelpCursors
