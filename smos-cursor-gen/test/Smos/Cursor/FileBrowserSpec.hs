{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.FileBrowserSpec where

import Cursor.Simple.DirForest
import qualified Data.ByteString as SB
import qualified Data.DirForest as DF
import Path
import Path.IO
import Smos.Cursor.FileBrowser
import Smos.Cursor.FileBrowser.Gen ()
import Smos.Data
import Smos.Data.Gen ()
import Smos.Undo
import Test.Hspec
import Test.Hspec.QuickCheck
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
  modifyMaxSuccess (`div` 10) $ describe "fileBrowserArchiveFile" $ it "correctly archives a file" $ forAllValid $ \rd ->
    forAllValid $ \rf ->
      forAllValid $ \sf ->
        withSystemTempDir "smos-cursor-test" $ \wd ->
          withSystemTempDir "smos-cursor-test" $ \ad -> do
            let base = wd </> rd
            let cts = smosFileYamlBS sf
            let af = base </> rf
            let fbc =
                  FileBrowserCursor
                    { fileBrowserCursorBase = base,
                      fileBrowserCursorDirForestCursor = makeDirForestCursor $ DF.singletonFile rf (),
                      fileBrowserCursorUndoStack = emptyUndoStack
                    }
            -- Put the file to archive in place
            ensureDir (parent af)
            SB.writeFile (fromAbsFile af) cts
            -- Do the archiving
            fbc' <- fileBrowserArchiveFile wd ad fbc
            shouldBeValid fbc'
            -- Check that the file was archived
            afs <- snd <$> listDirRecur ad
            case afs of
              [_] -> pure ()
              _ -> expectationFailure $ unlines $ ("The file was archived incorrectly, we didn't find exactly one file but " <> show (length afs) <> " instead:") : map fromAbsFile afs
            fileBrowserCursorDirForestCursor fbc' `shouldBe` Nothing
