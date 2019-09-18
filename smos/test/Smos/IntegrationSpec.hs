module Smos.IntegrationSpec
  ( spec
  ) where

import TestImport

import Smos.Data

import Smos

spec :: Spec
spec =
  describe "Persistence" $ do
    it "Does not create a file if a nonexistent file is not changed" $
      withSystemTempDir "smos-test" $ \d -> do
        p <- resolveFile d "test.smos"
        saveSmosFile emptySmosFile Nothing p
        b' <- doesFileExist p
        b' `shouldBe` False
    it
      "Does not create a file if an empty file is not changed and did not exist yet" $
      withSystemTempDir "smos-test" $ \d -> do
        p <- resolveFile d "test.smos"
        writeFile (toFilePath p) mempty
        saveSmosFile emptySmosFile Nothing p
        b' <- doesFileExist p
        b' `shouldBe` False
