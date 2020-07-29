{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.EditorSpec where

import Smos.Cursor.Editor.Gen ()
import Smos.Data.Gen ()
import Smos.Types
import Test.Hspec
import Test.Validity
import Test.Validity.Optics

spec :: Spec
spec = do
  genValidSpec @EditorCursor
  describe "makeEditorCursor"
    $ it "produces valid cursors"
    $ producesValidsOnValids2 makeEditorCursor
  describe "rebuildEditorCursor" $ do
    it "produces valid cursors" $ producesValidsOnValids rebuildEditorCursor
    it "roundtrips after makeEditorCursor" $ forAllValid $ \p -> forAllValid $ \msf ->
      let sfec = makeEditorCursor p msf
          (p', msf') = rebuildEditorCursor sfec
       in (p, msf) `shouldBe` (p', msf')
  describe "editorCursorSmosFileEditorCursorL" $ lensSpecOnValid editorCursorSmosFileEditorCursorL
  describe "editorCursorSelectionL" $ lensSpecOnValid editorCursorSelectionL
  describe "editorCursorDebugL" $ lensSpecOnValid editorCursorDebugL
  describe "editorCursorShowDebug"
    $ it "produces valid cursors"
    $ producesValidsOnValids editorCursorShowDebug
  describe "editorCursorHideDebug"
    $ it "produces valid cursors"
    $ producesValidsOnValids editorCursorHideDebug
  describe "editorCursorToggleDebug"
    $ it "produces valid cursors"
    $ producesValidsOnValids editorCursorToggleDebug
