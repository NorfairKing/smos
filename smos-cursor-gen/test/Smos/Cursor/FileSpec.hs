{-# LANGUAGE TypeApplications #-}
module Smos.Cursor.FileSpec where

import Test.Hspec
import Test.Validity

import Smos.Data.Gen ()

import Smos.Cursor.File
import Smos.Cursor.File.Gen ()

spec :: Spec
spec = do
    eqSpec @FileCursor
    genValidSpec @FileCursor
    describe "makeSmosFileCursor" $
        it "produces valid cursors" $ producesValidsOnValids makeSmosFileCursor
    describe "rebuildSmosFileCursor" $ do
        it "produces valid cursors" $ producesValidsOnValids rebuildSmosFileCursor
        it "is the inverse of makeFileCursor" $
            inverseFunctionsOnValid makeSmosFileCursor rebuildSmosFileCursor
