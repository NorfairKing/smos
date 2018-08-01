module Smos.Cursor.PropertiesSpec where

import Test.Hspec
import Test.Validity

import Smos.Data.Gen ()

import Smos.Cursor.Properties
import Smos.Cursor.Properties.Gen ()

spec :: Spec
spec = do
    describe "makePropertiesCursor" $
        it "produces valid cursors" $
        producesValidsOnValids makePropertiesCursor
    describe "rebuildPropertiesCursor" $ do
        it "produces valid cursors" $
            producesValidsOnValids rebuildPropertiesCursor
        it "is the inverse of makePropertiesCursor" $
            inverseFunctionsOnValid makePropertiesCursor rebuildPropertiesCursor
