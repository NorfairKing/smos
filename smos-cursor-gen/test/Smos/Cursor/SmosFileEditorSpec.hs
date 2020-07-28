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
