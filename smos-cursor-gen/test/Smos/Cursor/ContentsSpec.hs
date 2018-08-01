{-# LANGUAGE TypeApplications #-}
module Smos.Cursor.ContentsSpec where

import Test.Hspec
import Test.Validity

import Smos.Data.Gen ()

import Smos.Cursor.Contents
import Smos.Cursor.Contents.Gen ()

spec :: Spec
spec = do
    eqSpec @ContentsCursor
    genValidSpec @ContentsCursor
    describe "makeContentsCursor" $
        it "produces valid cursors" $ producesValidsOnValids makeContentsCursor
    describe "rebuildContentsCursor" $ do
        it "produces valid cursors" $
            producesValidsOnValids rebuildContentsCursor
        it "is the inverse of makeContentsCursor" $
            inverseFunctionsOnValid makeContentsCursor rebuildContentsCursor
