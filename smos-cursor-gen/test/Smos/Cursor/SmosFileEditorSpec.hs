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
  describe "makeSmosFileCursor" $ it "works on any valid smos file" $ forAllValid $ \sf ->
    forAllValid $ \rp ->
      withSystemTempDir "smos-cursor-test" $ \tdir -> do
        let p = tdir </> rp
        writeSmosFile p sf
        errOrCursor <- startSmosFileEditorCursor p
        case errOrCursor of
          Nothing -> expectationFailure "Locking should have been possible"
          Just (Left err) -> expectationFailure err
          Just (Right SmosFileEditorCursor {..}) -> smosFileEditorPath `shouldBe` p
