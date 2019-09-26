module Smos.Sync.Client.SaveSpec
  ( spec
  ) where

import qualified Data.Map as M

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Validity

import Smos.Sync.Client.Contents
import qualified Smos.Sync.Client.ContentsMap as CM
import Smos.Sync.Client.ContentsMap (ContentsMap(..))
import Smos.Sync.Client.OptParse.Types
import Smos.Sync.Client.Sync

import Smos.Sync.Client.ContentsMap.Gen
import Smos.Sync.Client.Sync.Gen ()
import Smos.Sync.Client.TestUtils

spec :: Spec
spec =
  withTestDir $
  describe "saveContentsMap" $ do
    describe "Any IgnoreFiles" $ do
      it "puts a single file in the right place with the right contents" $ \d ->
        forAllValid $ \igf ->
          forAllValid $ \rf ->
            forAllValid $ \contents -> do
              let m = CM.singleton rf contents
              saveContentsMap igf d m
              assertContents d m
      it "puts any number of files in the right place with the right contents" $ \d ->
        forAllValid $ \igf ->
          forAllValid $ \m -> do
            saveContentsMap igf d m
            assertContents d m
    describe "IgnoreNothing" $
      it "removes any files that are not in the map" $ \d ->
        forAllValid $ \m1 ->
          forAll (mapWithAdditions m1) $ \m -> do
            setupContents d m
            saveContentsMap IgnoreNothing d m1
            assertContents d m1
    describe "IgnoreHiddenFiles" $
      it "leaves any hidden files, even if they are not in the map" $ \d ->
        forAllValid $ \m2 ->
          forAll (mapWithAdditions m2) $ \m1 ->
            cover False 10 "has any hidden files" $ do
              let hiddenFilesBefore = M.filterWithKey (\p _ -> isHidden p) $ contentsMapFiles m1
              setupContents d m1
              saveContentsMap IgnoreHiddenFiles d m2
              m3 <- readContents d
              let hiddenFilesAfter = M.filterWithKey (\p _ -> isHidden p) $ contentsMapFiles m3
              hiddenFilesAfter `shouldBe` hiddenFilesBefore
