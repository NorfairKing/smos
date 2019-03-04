{-# LANGUAGE TypeApplications #-}

module Smos.IntegrationSpec
    ( spec
    ) where

import TestImport

import Smos.Data

import Smos

spec :: Spec
spec = do
    describe "Persistence" $
        it "Does not create a file if an empty file is not changed" $
            withSystemTempFile "smos-test" $ \p _ -> do
                b <- doesFileExist p
                b `shouldBe` False
                saveSmosFile emptySmosFile Nothing p
                b' <- doesFileExist p
                b' `shouldBe` False
