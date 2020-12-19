{-# LANGUAGE RecordWildCards #-}

module Smos.Cursor.SmosFileEditorSpec where

import Control.Monad
import Control.Monad.IO.Class
import Data.GenValidity.Path ()
import Path
import Path.IO
import Smos.Cursor.SmosFileEditor
import Smos.Data
import Smos.Data.Gen ()
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity
import UnliftIO.Resource

spec :: Spec
spec = modifyMaxShrinks (const 1) $ do
  describe "startSmosFileCursor" $ do
    it "works on any valid smos file" $
      forAllValid $ \sf ->
        -- to make sure paths are not too big
        forAllShrink (scale (`div` 5) genValid) shrinkValid $ \rp ->
          withSystemTempDir "smos-cursor-test" $ \tdir -> do
            let p = tdir </> rp
            writeSmosFile p sf
            runResourceT $ do
              errOrCursor <- startSmosFileEditorCursor p
              liftIO $ case errOrCursor of
                Nothing -> expectationFailure "Locking should have been possible"
                Just (Left err) -> expectationFailure err
                Just (Right SmosFileEditorCursor {..}) -> smosFileEditorPath `shouldBe` p
    it "creates a history with an empty starting point for a nonexistent file" $
      withSystemTempDir "smos-test" $ \d -> do
        p <- resolveFile d "test.smos" :: IO (Path Abs File)
        runResourceT $ do
          errOrCursor <- startSmosFileEditorCursor p
          liftIO $ case errOrCursor of
            Nothing -> expectationFailure "Locking should have been possible"
            Just (Left err) -> expectationFailure err
            Just (Right sfec) -> smosFileEditorStartingPoint sfec `shouldBe` Nothing
  describe "smosFileEditorCursorSave" $ do
    it "does not create a file if a nonexistent file is not changed" $
      withSystemTempDir "smos-test" $ \d -> do
        p <- resolveFile d "test.smos" :: IO (Path Abs File)
        runResourceT $ do
          errOrCursor <- startSmosFileEditorCursor p
          liftIO $ case errOrCursor of
            Nothing -> expectationFailure "Locking should have been possible"
            Just (Left err) -> expectationFailure err
            Just (Right sfec) -> do
              void $ smosFileEditorCursorSave sfec
        doesFileExist p `shouldReturn` False
  describe "saveSmosFile" $ do
    it "does not create a file if a nonexistent file is not changed" $
      withSystemTempDir "smos-test" $
        \d -> do
          p <- resolveFile d "test.smos"
          saveSmosFile emptySmosFile Nothing p
          doesFileExist p `shouldReturn` False
    it "does not create a file if an empty file is not changed and did not exist yet" $
      withSystemTempDir "smos-test" $
        \d -> do
          p <- resolveFile d "test.smos"
          writeFile (toFilePath p) mempty
          saveSmosFile emptySmosFile Nothing p
          doesFileExist p `shouldReturn` False
