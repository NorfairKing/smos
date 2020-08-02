module Smos.Cursor.EditorSpec where

import Data.GenValidity.Path ()
import Path
import Path.IO
import Smos.Data
import Smos.Data.Gen ()
import Smos.Types
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Validity

spec :: Spec
spec =
  modifyMaxShrinks (const 1) $ do
    describe "startEditorCursor" $ it "works on any valid smos file" $ forAllValid $ \sf ->
      forAllValid $ \rp ->
        withSystemTempDir "smos-test" $ \tdir -> do
          let p = tdir </> rp
          writeSmosFile p sf
          errOrCursor <- startEditorCursor p
          case errOrCursor of
            Nothing -> expectationFailure "Locking should have been possible"
            Just (Left err) -> expectationFailure err
            Just (Right _) -> pure ()
