{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.FileBrowserSpec where

import Cursor.Simple.DirForest
import Cursor.Types
import qualified Data.ByteString as SB
import qualified Data.DirForest as DF
import Data.Maybe
import Path
import Path.IO
import Smos.Cursor.FileBrowser
import Smos.Cursor.FileBrowser.Gen ()
import Smos.Data
import Smos.Data.Gen ()
import Smos.Undo
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @FileBrowserCursor
  describe "makeFileBrowserCursor" $
    it "produces valid cursors" $
      producesValidsOnValids2 makeFileBrowserCursor
  describe "rebuildFileBrowserCursor" $ do
    it "produces valid cursors" $ producesValidsOnValids rebuildFileBrowserCursor
    it "is the inverse of makeFileBrowserCursor" $
      forAllValid $
        \base -> inverseFunctionsOnValid (makeFileBrowserCursor base) rebuildFileBrowserCursor
  describe "fileBrowserCursorSelectNext" $
    it "produces valid cursors" $
      producesValidsOnValids fileBrowserCursorSelectNext
  describe "fileBrowserCursorSelectPrev" $
    it "produces valid cursors" $
      producesValidsOnValids fileBrowserCursorSelectPrev
  describe "fileBrowserCursorStartNew" $ it "produces valid cursors" $ producesValidsOnValids fileBrowserCursorStartNew
  describe "fileBrowserCursorStartNewBelowAtStart" $ it "produces valid cursors" $ producesValidsOnValids fileBrowserCursorStartNewBelowAtStart
  describe "fileBrowserCursorStartNewBelowAtEnd" $ it "produces valid cursors" $ producesValidsOnValids fileBrowserCursorStartNewBelowAtEnd
  describe "fileBrowserCursorStopNew" $ it "produces valid cursors" $ producesValidsOnValids fileBrowserCursorStopNew
  describe "fileBrowserCursorInsertChar" $ it "produces valid cursors" $ producesValidsOnValids2 fileBrowserCursorInsertChar
  describe "fileBrowserCursorAppendChar" $ it "produces valid cursors" $ producesValidsOnValids2 fileBrowserCursorAppendChar
  describe "fileBrowserCursorRemoveChar" $ it "produces valid cursors" $ producesValidsOnValids fileBrowserCursorRemoveChar
  describe "fileBrowserCursorDeleteChar" $ it "produces valid cursors" $ producesValidsOnValids fileBrowserCursorDeleteChar
  describe "fileBrowserCursorSelectPrevChar" $ it "produces valid cursors" $ producesValidsOnValids fileBrowserCursorSelectPrevChar
  describe "fileBrowserCursorSelectNextChar" $ it "produces valid cursors" $ producesValidsOnValids fileBrowserCursorSelectNextChar
  describe "fileBrowserCursorToggle" $
    it "produces valid cursors" $
      producesValidsOnValids fileBrowserCursorToggle
  describe "fileBrowserCursorToggleRecursively" $
    it "produces valid cursors" $
      producesValidsOnValids fileBrowserCursorToggleRecursively
  describe "fileBrowserRmEmptyDir" $
    it "produces the same result as if I had reread the cursor" $
      withSystemTempDir "smos-cursor-test-filebrowser" $
        \tdir -> do
          dirToDelete <- resolveDir tdir "dir-to-delete"
          ensureDir dirToDelete
          fbc <- startFileBrowserCursor tdir
          fbc' <- fileBrowserRmEmptyDir fbc
          fbc'' <- startFileBrowserCursor tdir
          rebuildFileBrowserCursor fbc' `shouldBe` rebuildFileBrowserCursor fbc''
  modifyMaxSuccess (`div` 10) $
    describe "fileBrowserArchiveFile" $ do
      let archiveTest rf =
            forAllValid $ \relativeDirInBase ->
              forAllValid $ \sf ->
                withSystemTempDir "smos-cursor-test" $ \wd ->
                  withSystemTempDir "smos-cursor-test" $ \ad -> do
                    let base = wd </> relativeDirInBase
                    let cts = smosFileYamlBS sf
                    let af = base </> rf
                    let goDown c = case dirForestCursorSelectFirstChild c of
                          Deleted -> Nothing
                          Updated Nothing -> Just c
                          Updated (Just c') -> goDown c'
                    let dfc :: Maybe (DirForestCursor ())
                        dfc = ((\c -> fromMaybe c (dirForestCursorOpenRecursively c)) <$> makeDirForestCursor (DF.singletonFile rf ())) >>= goDown
                    dfc `shouldSatisfy` isJust -- Sanity test
                    let fbc =
                          FileBrowserCursor
                            { fileBrowserCursorBase = base,
                              fileBrowserCursorDirForestCursor = dfc,
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
                      [] -> expectationFailure "The file was archived incorrectly, we didn't find any files in the archive."
                      [_] -> pure ()
                      _ -> expectationFailure $ unlines $ ("The file was archived incorrectly, we didn't find exactly one file but " <> show (length afs) <> " instead:") : map fromAbsFile afs
                    DF.toFileList (rebuildFileBrowserCursor fbc') `shouldBe` []
      it "correctly archives a file" $ forAllValid $ \rf -> archiveTest rf
      it "correctly archives a file in a subdir" $ forAllValid $ \rd -> forAllValid $ \rf -> archiveTest (rd </> rf)
      it "correctly archives a file in a nested subdir" $
        forAllValid $ \rd1 ->
          forAllValid $ \rd2 ->
            forAllValid $ \rf -> archiveTest (rd1 </> rd2 </> rf)
