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
      producesValid (makeCollapseEntry @Rational)
  describe "rebuildCollapseEntry" $ do
    it "produces valid values" $ producesValid (rebuildCollapseEntry @Rational)
    it "is the inverse of makeCollapseEntry" $
      inverseFunctions (makeCollapseEntry @Rational) (rebuildCollapseEntry @Rational)
  describe "collapseEntryValueL" $ lensSpec (collapseEntryValueL @Rational)
  describe "collapseEntryShowContentsL" $ lensSpec (collapseEntryShowContentsL @Rational)
  describe "collapseEntryShowHistoryL" $ lensSpec (collapseEntryShowHistoryL @Rational)
  describe "collapseEntryShowLogbookL" $ lensSpec (collapseEntryShowLogbookL @Rational)
  describe "collapseEntryShowTimestampsL" $ lensSpec (collapseEntryShowTimestampsL @Rational)
  describe "collapseEntrySetShowAll" $
    it "produces valid collapses" $
      producesValid2 (collapseEntrySetShowAll @Rational)
