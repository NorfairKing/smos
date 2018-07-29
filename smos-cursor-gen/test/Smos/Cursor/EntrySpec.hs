module Smos.Cursor.EntrySpec where

import Test.Hspec
import Test.Validity

import Smos.Data.Gen ()

import Smos.Cursor.Entry
import Smos.Cursor.Entry.Gen ()

spec :: Spec
spec = do
    describe "makeEntryCursor" $
        it "produces valid cursors" $ producesValidsOnValids makeEntryCursor
    describe "rebuildEntryCursor" $ do
     it "produces valid entries" $
        producesValidsOnValids
            rebuildEntryCursor
     it
            "is the inverse of makeEntryCursor" $
        inverseFunctionsOnValid makeEntryCursor rebuildEntryCursor
