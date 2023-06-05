{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.HelpSpec where

import Data.List
import Smos.Cursor.Help.Gen ()
import Smos.Data.Gen ()
import Smos.Types
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @HelpCursor
  -- Can't test this
  -- describe "makeHelpCursor" $ it "produces valid cursors" $ producesValid2 makeHelpCursor
  describe "searchHelpCursor" $ do
    it "produces valid search results" $ producesValid2 searchHelpCursor
    it "selects a subset of the available KeyHelpCursors" $
      forAllValid $
        \q ->
          forAllValid $ \khcs -> searchHelpCursor q khcs `shouldSatisfy` (`isSubsequenceOf` khcs)
  describe "helpCursorUp" $ it "produces valid cursors" $ producesValid helpCursorUp
  describe "helpCursorDown" $ it "produces valid cursors" $ producesValid helpCursorDown
  describe "helpCursorStart" $ it "produces valid cursors" $ producesValid helpCursorStart
  describe "helpCursorEnd" $ it "produces valid cursors" $ producesValid helpCursorEnd
  describe "helpCursorSelectHelp" $
    it "produces valid cursors" $
      producesValid helpCursorSelectHelp
  describe "helpCursorSelectSearch" $
    it "produces valid cursors" $
      producesValid helpCursorSelectSearch
  describe "helpCursorToggleSelection" $
    it "produces valid cursors" $
      producesValid helpCursorToggleSelection
  describe "helpCursorInsert" $
    it "produces valid cursors" $
      producesValid2 helpCursorInsert
  describe "helpCursorAppend" $
    it "produces valid cursors" $
      producesValid2 helpCursorAppend
  describe "helpCursorRemove" $
    it "produces valid cursors" $
      producesValid helpCursorRemove
  describe "helpCursorDelete" $
    it "produces valid cursors" $
      producesValid helpCursorDelete
  genValidSpec @KeyHelpCursor
  describe "combineKeyHelpCursors" $
    it "produces valid cursors" $
      producesValid combineKeyHelpCursors
