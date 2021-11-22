{-# LANGUAGE QuasiQuotes #-}

module Smos.ArchiveSpec
  ( spec,
  )
where

import Data.Time
import Path
import Smos.Archive
import Smos.Data.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "prepareToArchive" $ do
    it "produces valid smos files" $ producesValid2 prepareToArchive
  describe "destinationFile" $ do
    it "outputs the same as last time" $ do
      goldenStringFile "test_resources/destination.txt" $
        fromAbsFile
          <$> destinationFile
            (LocalTime (fromGregorian 2021 04 23) (TimeOfDay 07 20 20))
            [absdir|/home/user/workflow|]
            [absdir|/home/user/archive|]
            [absfile|/home/user/workflow/file/to/archive.smos|]
