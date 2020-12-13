{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.CollapseSpec where

import Smos.Cursor.Collapse
import Smos.Cursor.Collapse.Gen ()
import Smos.Data.Gen ()
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Lens

spec :: Spec
spec = do
  genValidSpec @(CollapseEntry Rational)
  describe "makeCollapseEntry" $
    it "produces valid collapse's" $
      producesValidsOnValids (makeCollapseEntry @Rational)
  describe "rebuildCollapseEntry" $ do
    it "produces valid values" $ producesValidsOnValids (rebuildCollapseEntry @Rational)
    it "is the inverse of makeCollapseEntry" $
      inverseFunctionsOnValid (makeCollapseEntry @Rational) (rebuildCollapseEntry @Rational)
  describe "collapseEntryValueL" $ lensSpecOnValid (collapseEntryValueL @Rational)
  describe "collapseEntryShowContentsL" $ lensSpecOnValid (collapseEntryShowContentsL @Rational)
  describe "collapseEntryShowHistoryL" $ lensSpecOnValid (collapseEntryShowHistoryL @Rational)
  describe "collapseEntryShowLogbookL" $ lensSpecOnValid (collapseEntryShowLogbookL @Rational)
  describe "collapseEntryShowTimestampsL" $ lensSpecOnValid (collapseEntryShowTimestampsL @Rational)
  describe "collapseEntrySetShowAll" $
    it "produces valid collapses" $
      producesValidsOnValids2 (collapseEntrySetShowAll @Rational)
