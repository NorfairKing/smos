module Smos.Cursor.LogbookSpec where

import Test.Hspec
import Test.Validity

import Smos.Data.Gen ()

import Smos.Cursor.Logbook
import Smos.Cursor.Logbook.Gen ()

spec :: Spec
spec = do
    describe "makeLogbookCursor" $
        it "produces valid cursors" $ producesValidsOnValids makeLogbookCursor
    describe "rebuildLogbookCursor" $ do
        it "produces valid cursors" $
            producesValidsOnValids rebuildLogbookCursor
        it "is the inverse of makeLogbookCursor" $
            inverseFunctionsOnValid makeLogbookCursor rebuildLogbookCursor
