{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.FileBrowserSpec where

import Path.IO
import Smos.Cursor.FileBrowser
import Smos.Cursor.FileBrowser.Gen ()
import Smos.Data.Gen ()
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  genValidSpec @FileBrowserCursor
  describe "makeFileBrowserCursor"
    $ it "produces valid cursors"
    $ producesValidsOnValids2 makeFileBrowserCursor
  describe "rebuildFileBrowserCursor" $ do
    it "produces valid cursors" $ producesValidsOnValids rebuildFileBrowserCursor
    it "is the inverse of makeFileBrowserCursor"
      $ forAllValid
      $ \base -> inverseFunctionsOnValid (makeFileBrowserCursor base) rebuildFileBrowserCursor
  describe "fileBrowserCursorSelectNext"
    $ it "produces valid cursors"
    $ producesValidsOnValids fileBrowserCursorSelectNext
  describe "fileBrowserCursorSelectPrev"
    $ it "produces valid cursors"
    $ producesValidsOnValids fileBrowserCursorSelectPrev
  describe "fileBrowserCursorToggle"
    $ it "produces valid cursors"
    $ producesValidsOnValids fileBrowserCursorToggle
  describe "fileBrowserCursorToggleRecursively"
    $ it "produces valid cursors"
    $ producesValidsOnValids fileBrowserCursorToggleRecursively
  describe "fileBrowserRmEmptyDir"
    $ it "produces the same result as if I had reread the cursor"
    $ withSystemTempDir "smos-cursor-test-filebrowser"
    $ \tdir -> do
      dirToDelete <- resolveDir tdir "dir-to-delete"
      ensureDir dirToDelete
      fbc <- startFileBrowserCursor tdir
      fbc' <- fileBrowserRmEmptyDir fbc
      fbc'' <- startFileBrowserCursor tdir
      rebuildFileBrowserCursor fbc' `shouldBe` rebuildFileBrowserCursor fbc''
