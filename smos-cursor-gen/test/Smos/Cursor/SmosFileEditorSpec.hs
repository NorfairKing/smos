{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.SmosFileEditorSpec where

import Data.Maybe
import Smos.Cursor.SmosFileEditor
import Smos.Cursor.SmosFileEditor.Gen ()
import Smos.Data.Gen ()
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Validity
import Test.Validity.Optics

spec :: Spec
spec = modifyMaxShrinks (const 1) $ do
  genValidSpec @SmosFileEditorCursor
  describe "makeSmosFileCursor" $ it "produces valid results" $ producesValidsOnValids2 makeSmosFileEditorCursor
  describe "rebuildSmosFileCursor" $ do
    it "produces valid results" $ producesValidsOnValids rebuildSmosFileEditorCursor
    it "roundtrips after makeSmosFileCursor" $ forAllValid $ \p -> forAllValid $ \msf ->
      let sfec = makeSmosFileEditorCursor p msf
          (p', msf') = rebuildSmosFileEditorCursor sfec
       in (p, msf) `shouldBe` (p', msf')
