{-# LANGUAGE RecordWildCards #-}

module Smos.Cursor.SmosFileEditorSpec where

import Data.Either
import Data.GenValidity.Path ()
import Data.Maybe
import Path
import Path.IO
import Smos.Cursor.SmosFileEditor
import Smos.Data
import Smos.Data.Gen ()
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Validity
import Test.Validity.Optics

spec :: Spec
spec = modifyMaxShrinks (const 1) $ do
  describe "startSmosFileCursor" $ it "works on any valid smos file" $ forAllValid $ \sf ->
    forAllValid $ \rp ->
      withSystemTempDir "smos-cursor-test" $ \tdir -> do
        let p = tdir </> rp
        writeSmosFile p sf
        errOrCursor <- startSmosFileEditorCursor p
        case errOrCursor of
          Nothing -> expectationFailure "Locking should have been possible"
          Just (Left err) -> expectationFailure err
          Just (Right SmosFileEditorCursor {..}) -> smosFileEditorPath `shouldBe` p
  describe "saveSmosFile" $ do
    it "Does not create a file if a nonexistent file is not changed"
      $ withSystemTempDir "smos-test"
      $ \d -> do
        p <- resolveFile d "test.smos"
        saveSmosFile emptySmosFile Nothing p
        b' <- doesFileExist p
        b' `shouldBe` False
    it "Does not create a file if an empty file is not changed and did not exist yet"
      $ withSystemTempDir "smos-test"
      $ \d -> do
        p <- resolveFile d "test.smos"
        writeFile (toFilePath p) mempty
        saveSmosFile emptySmosFile Nothing p
        b' <- doesFileExist p
        b' `shouldBe` False
